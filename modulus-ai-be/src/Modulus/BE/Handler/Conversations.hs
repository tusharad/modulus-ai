module Modulus.BE.Handler.Conversations
  ( conversationsServer
  , getConversationsHandler
  , getConversationMessagesHandler
  , addConversationHandler
  , addConversationMessageHandler
  , getLLMRespStreamHandler
  , deleteConversationHandler
  ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.V4 (nextRandom)
import Langchain.LLM.Core (StreamHandler (..))
import qualified Langchain.LLM.Core as Langchain
import Langchain.Utils (showText)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , addMsgAttachment
  , getChatMessagesWithAttachmentsByConvID
  , getFirst10MessagesByConvIDAfterMsgID
  )
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.LLM
import Modulus.BE.Log (logDebug)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error
import Modulus.BE.Monad.Storage
import Modulus.BE.Service.Conversation (updateConversationTitle)
import Modulus.Common.Types
import Modulus.Common.Utils (takeRecentMessages)
import qualified Orville.PostgreSQL as Orville
import Servant
import Servant.Multipart
import qualified Servant.Types.SourceT as S
import System.Directory
import System.FilePath
import UnliftIO.Async (concurrently)
import qualified UnliftIO.Concurrent as UnliftIO (forkIO)

conversationsServer :: ServerT ConversationsAPI AppM
conversationsServer =
  addConversationHandler
    :<|> getConversationsHandler
    :<|> addConversationMessageHandler
    :<|> getConversationMessagesHandler
    :<|> getLLMRespStreamHandler
    :<|> deleteConversationHandler
    :<|> getModelProvidersHandler
    :<|> getPaginatedMessagesHandler

getPaginatedMessagesHandler ::
  AuthResult -> ConversationPublicID -> Maybe ChatMessageID -> AppM [ChatMessageWithAttachments]
getPaginatedMessagesHandler (Authenticated user _) convPublicId mAfterMsgID = do
  convRead <- getConvRead user convPublicId
  getFirst10MessagesByConvIDAfterMsgID (conversationID convRead) mAfterMsgID
getPaginatedMessagesHandler _ _ _ =
  throwError $ AuthenticationError "Invalid token"

getModelProvidersHandler :: AppM [ModelProviders]
getModelProvidersHandler = do
  x <- asks configCurrentProviders
  logDebug $ "sending providers " <> T.pack (show x)
  pure x

deleteConversationHandler :: AuthResult -> ConversationPublicID -> AppM ()
deleteConversationHandler (Authenticated user _subscription) convPublicId = do
  convRead <- getConvRead user convPublicId
  deleteConversation (conversationID convRead)
deleteConversationHandler _ _ = throwError $ AuthenticationError "Invalid token"

toLangchainRole :: MessageRole -> Langchain.Role
toLangchainRole r = case r of
  MessageRoleUser -> Langchain.User
  MessageRoleAssistant -> Langchain.Assistant
  MessageRoleSystem -> Langchain.System
  MessageRoleTool -> Langchain.Tool

safeReadFile :: FilePath -> IO (Either IOException BS.ByteString)
safeReadFile = try . BS.readFile

asPath :: FilePath -> IO (Maybe BS.ByteString)
asPath filePath = do
  exists <- doesFileExist filePath
  if exists
    then either (const Nothing) Just <$> safeReadFile filePath
    else return Nothing

supportedExtensions :: [String]
supportedExtensions = [".jpg", ".jpeg", ".png"]

isSupportedExtension :: FilePath -> Bool
isSupportedExtension p = map toLower (takeExtension p) `elem` supportedExtensions

encodeImage :: Text -> Text -> FilePath -> IO (Maybe Text)
encodeImage provider fileType filePath = do
  if not (isSupportedExtension filePath)
    then return Nothing
    else do
      maybeContent <- asPath filePath
      let res = fmap (TE.decodeUtf8 . Base64.encode) maybeContent
      if provider == "ollama"
        then
          return res
        else
          return $
            ( \encodedImageData ->
                "data:image/"
                  <> T.drop 6 fileType -- dropping image/
                  <> ";base64,"
                  <> encodedImageData
            )
              <$> res

getLLMRespStreamHandler ::
  AuthResult ->
  ConversationPublicID ->
  LLMRespStreamBody ->
  AppM (SourceIO LLMRespStream)
getLLMRespStreamHandler authUser convPublicId streamBody@LLMRespStreamBody {..} = do
  logDebug $ "Streaming response for " <> T.pack (show convPublicId)
  cLst <- getConversationMessagesHandler authUser convPublicId
  case NE.nonEmpty cLst of
    Nothing -> throwError $ NotFoundError "Empty conversation"
    Just chatMsgLst_ -> do
      when
        (length chatMsgLst_ < 2)
        ( void $ UnliftIO.forkIO $ do
            updateConversationTitle convPublicId streamBody
        )
      let (chatMsgLst, remainingMsgs) = takeRecentMessages 5000 chatMsgLst_
      -- Converting ModulusAI's Message type into Langchain's Message type
      -- Along with conversation, also finding if there are any image attachments
      -- If yes, encoding it as base64 image and attaching it as part of MessageImages.
      msgList_ <-
        mapM
          ( \chatMsg -> do
              let imageAttachments =
                    filter
                      ( \m ->
                          "image/"
                            `T.isPrefixOf` messageAttachmentFileType m
                      )
                      (mas chatMsg)
              encodedImagesMaybes <-
                mapM
                  ( \m ->
                      liftIO
                        . encodeImage provider (messageAttachmentFileType m)
                        . T.unpack
                        $ messageAttachmentStoragePath m
                  )
                  imageAttachments
              let encodedImages = catMaybes encodedImagesMaybes
              unless (null encodedImages) $
                logDebug $
                  "Attaching "
                    <> showText (length encodedImages)
                    <> " image(s) to conversation "
                    <> convPublicId
                    <> ", message ID "
                    <> showText (chatMessageId $ cm chatMsg)
              pure $
                Langchain.Message
                  (toLangchainRole $ chatMessageRole $ cm chatMsg)
                  (chatMessageContent $ cm chatMsg)
                  ( Langchain.defaultMessageData
                      { Langchain.messageImages = Just encodedImages
                      }
                  )
          )
          chatMsgLst
      -- Filtering attachments whose type is image, since images are being attached in the message type itself.
      msgListWithoutSummarizedHistory <- case ( filter
                                                  ( \m ->
                                                      not $
                                                        "image/"
                                                          `T.isPrefixOf` messageAttachmentFileType m
                                                  )
                                                  . mconcat
                                                  . NE.toList
                                              )
        $ mas <$> chatMsgLst of
        [] -> do
          logDebug $ "no attachments found" <> T.pack (show chatMsgLst)
          pure msgList_
        msgAttachmentsList -> do
          logDebug $ "sending attachments " <> T.pack (show msgAttachmentsList)
          attachDocumentRAG msgAttachmentsList msgList_ streamBody
      msgList <- case remainingMsgs of
        [] -> pure msgListWithoutSummarizedHistory
        _ -> do
          logDebug "summarizing history"
          eSummarizedMsg <- summarizeConversationHistory streamBody remainingMsgs
          case eSummarizedMsg of
            Left err -> do
              logDebug $ "Failed to summarize older messages: " <> showText err
              pure msgListWithoutSummarizedHistory
            Right summarizedMsg -> do
              logDebug $ "summarized history " <> T.pack (show summarizedMsg)
              pure $ msgListWithoutSummarizedHistory <> (summarizedMsg NE.:| [])
      tokenChan <- liftIO newChan
      let st =
            StreamHandler
              { onToken = writeChan tokenChan . Just
              , onComplete = writeChan tokenChan Nothing
              }
      eLLM <- mkLLMProvider streamBody
      case eLLM of
        Left err -> throwError $ ValidationError err
        Right llmProvider -> do
          logDebug $ "Provider selected: " <> provider
          void . UnliftIO.forkIO . liftIO . void $
            streamWithProvider
              llmProvider
              msgList
              st
              toolCall
      pure $ chanSource tokenChan
  where
    chanSource :: Chan (Maybe Text) -> SourceIO LLMRespStream
    chanSource chan =
      S.fromStepT loop
      where
        loop = S.Effect $ do
          mbToken <- readChan chan
          pure $ case mbToken of
            Nothing -> S.Stop
            Just text -> S.Yield (LLMRespStream text Nothing Nothing) loop

getConvRead :: UserRead -> ConversationPublicID -> AppM ConversationRead
getConvRead user convPublicId = do
  mbConversationRead <- getConversationsByPublicID convPublicId
  case mbConversationRead of
    Nothing -> throwError $ NotFoundError "Conversation not found"
    Just convRead -> do
      when
        (conversationUserID convRead /= Just (userID user))
        ( throwError $
            AuthorizationError
              "You are not authorized to see this conversation"
        )
      pure convRead

getConversationMessagesHandler ::
  AuthResult ->
  ConversationPublicID ->
  AppM [ChatMessageWithAttachments]
getConversationMessagesHandler (Authenticated user _subscription) convPublicId = do
  convRead <- getConvRead user convPublicId
  getChatMessagesWithAttachmentsByConvID (conversationID convRead)
getConversationMessagesHandler _ _ =
  throwError $
    AuthenticationError "Invalid token"

supportedFileTypes :: [Text]
supportedFileTypes =
  [ "text/plain"
  , "application/pdf"
  , "image/png"
  , "image/jpeg"
  ]

storeAttachmentIfExist ::
  Maybe (FileData Mem) ->
  AppM (Maybe AttachmentInfo)
storeAttachmentIfExist Nothing = pure Nothing
storeAttachmentIfExist (Just FileData {..}) = do
  logDebug $ "got attachment" <> fdFileName
  do
    if fdFileCType `notElem` supportedFileTypes
      then throwError $ ValidationError "Unsupported file type"
      else do
        uuid <- liftIO nextRandom
        let newAttachmentObjectName =
              "attachment_" <> show uuid <> takeExtension (T.unpack fdFileName)
        let fileSize = BSL.length fdPayload
        unless (fileSize < 1000000) $
          throwError $
            ValidationError "File to large :("
        storageConf <- mkStorageFromEnv
        fileUploadPath <- asks configFileUploadPath
        eRes <- saveFile storageConf newAttachmentObjectName fdPayload
        case eRes of
          Left err -> do
            throwError $ ValidationError $ "Could not upload file: " <> showText err
          Right _ ->
            pure $
              Just $
                AttachmentInfo
                  { attachmentName = T.pack newAttachmentObjectName
                  , attachmentType = fdFileCType
                  , attachmentSize = fileSize
                  , attachmentUploadPath = fileUploadPath </> newAttachmentObjectName
                  }

data AttachmentInfo = AttachmentInfo
  { attachmentName :: Text
  , attachmentType :: Text
  , attachmentSize :: Int64
  , attachmentUploadPath :: FilePath
  }

addConversationMessageHandler ::
  AuthResult ->
  ConversationPublicID ->
  AddMessageRequest ->
  AppM ()
addConversationMessageHandler
  (Authenticated user _subscription)
  convPublicId
  AddMessageRequest {..} = do
    (convRead, mbAttachmentInfo) <-
      concurrently
        (getConvRead user convPublicId)
        (storeAttachmentIfExist addMessageAttachment)
    let role = case addMessageRole of
          "user" -> MessageRoleUser
          "assistant" -> MessageRoleAssistant
          "system" -> MessageRoleSystem
          "tool" -> MessageRoleTool
          _ -> MessageRoleUser
    let chatMsgWrite =
          ChatMessage
            { chatMessageID = ()
            , chatMessagePublicID = ()
            , chatMessageConversationID = conversationID convRead
            , chatMessageRole = role
            , chatMessageContent = messageContent
            , chatMessageModel = addMessageModel
            , chatMessageProvider = addMessageProvider
            , chatMessagePromptTokens = Nothing
            , chatMessageCompletionTokens = Nothing
            , chatMessageCreatedAt = ()
            }
    Orville.withTransaction $ do
      chatMsgRead <- addChatMessage chatMsgWrite
      case mbAttachmentInfo of
        Nothing -> pure ()
        Just AttachmentInfo {..} -> do
          let msgAttachWrite =
                MessageAttachment
                  { messageAttachmentID = ()
                  , messageAttachmentMessageID = chatMessageID chatMsgRead
                  , messageAttachmentFileName = attachmentName
                  , messageAttachmentFileType = attachmentType
                  , messageAttachmentFileSizeBytes = attachmentSize
                  , messageAttachmentStoragePath = T.pack attachmentUploadPath
                  , messageAttachmentCreatedAt = ()
                  }
          void $ addMsgAttachment msgAttachWrite
addConversationMessageHandler _ _ _ =
  throwError $ AuthenticationError "Invalid token"

addConversationHandler ::
  AuthResult -> AddConversationRequest -> AppM ConversationPublicID
addConversationHandler (Authenticated user _subscription) AddConversationRequest {..} = do
  let conversationWrite =
        Conversation
          { conversationID = ()
          , conversationPublicID = ()
          , conversationUserID = Just (userID user)
          , conversationTitle = conversationTitle
          , conversationCreatedAt = ()
          , conversationUpdatedAt = ()
          }
  conversationRead <- addConversation conversationWrite
  pure $ conversationPublicID conversationRead
addConversationHandler TokenExpired _ =
  throwError $ AuthenticationError "Token expired"
addConversationHandler _ _ = throwError $ AuthenticationError "Invalid token"

getConversationsHandler :: AuthResult -> AppM [ConversationRead]
getConversationsHandler (Authenticated user _subscription) = getConversationsByUserID (userID user)
getConversationsHandler TokenExpired = throwError $ AuthenticationError "Token expired"
getConversationsHandler _ = throwError $ AuthenticationError "Invalid token"
