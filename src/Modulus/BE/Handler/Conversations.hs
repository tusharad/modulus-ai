module Modulus.BE.Handler.Conversations
  ( conversationsServer
  , getConversationsHandler
  , getConversationMessagesHandler
  , addConversationHandler
  , addConversationMessageHandler
  , getLLMRespStreamHandler
  ) where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Langchain.LLM.Core (StreamHandler (..))
import qualified Langchain.LLM.Core as Langchain
import Langchain.LLM.Ollama (Ollama (..))
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
  ( ChatMessage (..)
  , ChatMessageRead
  , Conversation (..)
  , ConversationPublicID
  , ConversationRead
  , MessageRole (..)
  , User (userID)
  , UserRead
  )
import Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , getChatMessagesByConvID
  )
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error
import Servant
import qualified Servant.Types.SourceT as S
import Langchain.LLM.OpenAICompatible (mkOpenRouter)

conversationsServer :: ServerT ConversationsAPI AppM
conversationsServer =
  addConversationHandler
    :<|> getConversationsHandler
    :<|> addConversationMessageHandler
    :<|> getConversationMessagesHandler
    :<|> getLLMRespStreamHandler

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
    void $ addChatMessage chatMsgWrite
addConversationMessageHandler _ _ _ = throwError $ AuthenticationError "Invalid token"

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
