module Modulus.BE.Handler.Conversations
  ( conversationsServer
  ) where

import Modulus.BE.Api.Types (AddConversationRequest (..))
import Modulus.BE.Api.V1
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
  ( Conversation (..)
  , ConversationPublicID
  , ConversationRead
  , User (userID)
  )
import Modulus.BE.DB.Queries.Conversation
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error (AppError (AuthenticationError))
import Servant

conversationsServer :: ServerT ConversationsAPI AppM
conversationsServer =
  addConversationHandler
    :<|> getConversationsHandler

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
