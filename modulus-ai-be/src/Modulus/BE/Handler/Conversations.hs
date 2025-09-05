module Modulus.BE.Handler.Conversations
  ( conversationsServer
  , getConversationsHandler
  , getConversationMessagesHandler
  , addConversationHandler
  , addConversationMessageHandler
  , getLLMRespStreamHandler
  , deleteConversationHandler
  ) where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Langchain.LLM.Core (StreamHandler (..))
import qualified Langchain.LLM.Core as Langchain
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , addMsgAttachment
  , getChatMessagesWithAttachmentsByConvID
  )
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.LLM
import Modulus.BE.Log (logDebug)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error
import Modulus.BE.Monad.Storage
import Modulus.BE.Service.Conversation (updateConversationTitle)
import Modulus.Common.Types
import qualified Orville.PostgreSQL as Orville
import Servant
import Servant.Multipart
import qualified Servant.Types.SourceT as S
import System.FilePath
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

getModelProvidersHandler :: AppM [ModelProviders]
getModelProvidersHandler = do
  x <- asks configCurrentProviders
  logDebug $ "sending providers " <> T.pack (show x)
  pure x

deleteConversationHandler :: AuthResult -> ConversationPublicID -> AppM ()
deleteConversationHandler (Authenticated user) convPublicId = do
  convRead <- getConvRead user convPublicId
  deleteConversation (conversationID convRead)
deleteConversationHandler _ _ = throwError $ AuthenticationError "Invalid token"

toLangchainRole :: MessageRole -> Langchain.Role
toLangchainRole r = case r of
  MessageRoleUser -> Langchain.User
  MessageRoleAssistant -> Langchain.Assistant
  MessageRoleSystem -> Langchain.System
  MessageRoleTool -> Langchain.Tool

getLLMRespStreamHandler ::
  AuthResult ->
  ConversationPublicID ->
  LLMRespStreamBody ->
  AppM (SourceIO LLMRespStream)
getLLMRespStreamHandler authUser convPublicId streamBody@LLMRespStreamBody {..} = do
  chatMsgLst_ <- getConversationMessagesHandler authUser convPublicId
  case NE.nonEmpty chatMsgLst_ of
    Nothing -> throwError $ NotFoundError "Empty conversation"
    Just chatMsgLst -> do
      when
        (length chatMsgLst < 2)
        ( void $ UnliftIO.forkIO $ do
            updateConversationTitle convPublicId streamBody
        )
      let msgList_ =
            NE.map
              ( \ChatMessage {..} ->
                  Langchain.Message
                    (toLangchainRole chatMessageRole)
                    chatMessageContent
                    Langchain.defaultMessageData
              )
              (cm <$> chatMsgLst)
      msgList <- case (mconcat . NE.toList) $ mas <$> chatMsgLst of
        [] -> do
          logDebug $ "no chatMsgLst found" <> T.pack (show chatMsgLst)
          pure msgList_
        msgAttachmentsList -> attachDocumentRAG msgAttachmentsList msgList_ streamBody
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
          void $
            liftIO $
              forkIO $
                void $
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
getConversationMessagesHandler (Authenticated user) convPublicId = do
  convRead <- getConvRead user convPublicId
  getChatMessagesWithAttachmentsByConvID (conversationID convRead)
getConversationMessagesHandler _ _ =
  throwError $
    AuthenticationError "Invalid token"

supportedFileTypes :: [Text]
supportedFileTypes =
  [ "text/plain"
  , "application/pdf"
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
            throwError $ ValidationError $ "Could not upload file: " <> T.pack err
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
  (Authenticated user)
  convPublicId
  AddMessageRequest {..} = do
    convRead <- getConvRead user convPublicId
    mbAttachmentInfo <- storeAttachmentIfExist addMessageAttachment
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
addConversationHandler (Authenticated user) AddConversationRequest {..} = do
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
getConversationsHandler (Authenticated user) = getConversationsByUserID (userID user)
getConversationsHandler TokenExpired = throwError $ AuthenticationError "Token expired"
getConversationsHandler _ = throwError $ AuthenticationError "Invalid token"
