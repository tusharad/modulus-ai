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
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Langchain.LLM.Core (StreamHandler (..))
import qualified Langchain.LLM.Core as Langchain
import Langchain.LLM.Ollama (Ollama (..))
import Langchain.LLM.OpenAICompatible (mkOpenRouter)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
  ( ChatMessage (..)
  , ChatMessageRead
  , Conversation (..)
  , ConversationPublicID
  , ConversationRead
  , MessageAttachment (..)
  , MessageRole (..)
  , User (userID)
  , UserRead
  )
import Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , addMsgAttachment
  , getChatMessagesByConvID
  )
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.Log (logDebug)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error
import Modulus.Common.Types
import qualified Orville.PostgreSQL as Orville
import Servant
import Servant.Multipart
import qualified Servant.Types.SourceT as S
import System.FilePath

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
getModelProvidersHandler = asks configCurrentProviders

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
getLLMRespStreamHandler authUser convPublicId LLMRespStreamBody {..} = do
  chatMsgLst_ <- getConversationMessagesHandler authUser convPublicId
  case NE.nonEmpty chatMsgLst_ of
    Nothing -> throwError $ NotFoundError "Empty conversation"
    Just chatMsgLst -> do
      let msgList =
            NE.map
              ( \ChatMessage {..} ->
                  Langchain.Message
                    (toLangchainRole chatMessageRole)
                    chatMessageContent
                    Langchain.defaultMessageData
              )
              chatMsgLst
      tokenChan <- liftIO newChan
      let st =
            StreamHandler
              { onToken = writeChan tokenChan . Just
              , onComplete = writeChan tokenChan Nothing
              }
      case provider of
        "openrouter" -> do
          let openRouterLLM =
                mkOpenRouter
                  modelUsed
                  []
                  Nothing
                  (fromMaybe "" apiKey)
          void . liftIO . forkIO $
            void $
              Langchain.stream openRouterLLM msgList st Nothing
        "ollama" -> do
          let ollamaLLM = Ollama modelUsed []
          void . liftIO . forkIO $
            void $
              Langchain.stream ollamaLLM msgList st Nothing
        _ -> throwError $ NotFoundError "Provider not found"
      logDebug $ "Provider selected: " <> provider
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
  AppM [ChatMessageRead]
getConversationMessagesHandler (Authenticated user) convPublicId = do
  convRead <- getConvRead user convPublicId
  getChatMessagesByConvID (conversationID convRead)
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
        fPath <- asks configFileUploadPath
        let resultPath = fPath </> newAttachmentObjectName
        eRes <-
          liftIO $
            try $
              BSL.writeFile resultPath fdPayload
        case eRes of
          Left err -> do
            let errMsg = T.pack $ show (err :: SomeException)
            throwError $ ValidationError $ "Could not upload file: " <> errMsg
          Right _ ->
            pure $
              Just $
                AttachmentInfo
                  { attachmentName = T.pack newAttachmentObjectName
                  , attachmentType = fdFileCType
                  , attachmentSize = fileSize
                  , attachmentFilePath = T.pack resultPath
                  }

data AttachmentInfo = AttachmentInfo
  { attachmentName :: Text
  , attachmentType :: Text
  , attachmentSize :: Int64
  , attachmentFilePath :: Text
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
                  , messageAttachmentStoragePath = attachmentFilePath
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
addConversationHandler TokenExpired _ = throwError $ AuthenticationError "Token expired"
addConversationHandler _ _ = throwError $ AuthenticationError "Invalid token"

getConversationsHandler :: AuthResult -> AppM [ConversationRead]
getConversationsHandler (Authenticated user) = getConversationsByUserID (userID user)
getConversationsHandler TokenExpired = throwError $ AuthenticationError "Token expired"
getConversationsHandler _ = throwError $ AuthenticationError "Invalid token"
