module Modulus.BE.DB.Queries.Conversation
  ( addConversation
  , getConversationsByUserID
  , getConversationsByPublicID
  , deleteConversation
  ) where

import Modulus.BE.DB.Internal.Marshaller.Conversation
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (conversationTable)
import Orville.PostgreSQL

addConversation :: MonadOrville m => ConversationWrite -> m ConversationRead
addConversation = insertAndReturnEntity conversationTable

getConversationsByUserID :: MonadOrville m => UserID -> m [ConversationRead]
getConversationsByUserID userID =
  findEntitiesBy conversationTable $
    where_ (fieldEquals conversationUserIDField (Just userID))

getConversationsByPublicID ::
  MonadOrville m =>
  ConversationPublicID ->
  m (Maybe ConversationRead)
getConversationsByPublicID convPublicId =
  findFirstEntityBy conversationTable $
    where_ (fieldEquals conversationPublicIDField convPublicId)

deleteConversation :: MonadOrville m => ConversationID -> m ()
deleteConversation = deleteEntity conversationTable
