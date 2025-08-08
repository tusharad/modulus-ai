module Modulus.BE.Handler.Conversations
  ( conversationsServer
  ) where

import Control.Monad (void, when)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
  ( ChatMessage (..)
  , ChatMessageRead
  , Conversation (..)
  , ConversationPublicID
  , ConversationRead
  , User (userID)
  , UserRead
  )
import Modulus.BE.DB.Queries.ChatMessage (addChatMessage, getChatMessagesByConvID)
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error
import Servant

conversationsServer :: ServerT ConversationsAPI AppM
conversationsServer =
  addConversationHandler
    :<|> getConversationsHandler
    :<|> addConversationMessage
    :<|> getConversationMessages

getConvRead :: UserRead -> ConversationPublicID -> AppM ConversationRead
getConvRead user convPublicId = do
  mbConversationRead <- getConversationsByPublicID convPublicId
  case mbConversationRead of
    Nothing -> throwError $ NotFoundError "Conversation not found"
    Just convRead -> do
      when
        (conversationUserID convRead /= Just (userID user))
        (throwError $ AuthorizationError "You are not authorized to see this conversation")
      pure convRead

getConversationMessages :: AuthResult -> ConversationPublicID -> AppM [ChatMessageRead]
getConversationMessages (Authenticated user) convPublicId = do
  convRead <- getConvRead user convPublicId
  getChatMessagesByConvID (conversationID convRead)
getConversationMessages _ _ = throwError $ AuthenticationError "Invalid token"

addConversationMessage ::
  AuthResult ->
  ConversationPublicID ->
  AddMessageRequest ->
  AppM ()
addConversationMessage (Authenticated user) convPublicId AddMessageRequest {..} = do
  convRead <- getConvRead user convPublicId
  let chatMsgWrite =
        ChatMessage
          { chatMessageID = ()
          , chatMessagePublicID = ()
          , chatMessageConversationID = conversationID convRead
          , chatMessageRole = messageRole
          , chatMessageContent = messageContent
          , chatMessageModelUsed = Just messageModelUsed
          , chatMessageCreatedAt = ()
          }
  void $ addChatMessage chatMsgWrite
addConversationMessage _ _ _ = throwError $ AuthenticationError "Invalid token"

addConversationHandler :: AuthResult -> AddConversationRequest -> AppM ConversationPublicID
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
