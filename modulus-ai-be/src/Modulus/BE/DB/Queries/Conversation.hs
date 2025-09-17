module Modulus.BE.DB.Queries.Conversation
  ( addConversation
  , getConversationsByUserID
  , getConversationsByPublicID
  , deleteConversation
  , updateConversation
  , findSummaryByConvID
  , addSummary
  ) where

import Modulus.BE.DB.Internal.Marshaller (oldConvSummaryConversationIDField)
import Modulus.BE.DB.Internal.Marshaller.Conversation
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (conversationTable, oldConvSummaryTable)
import Orville.PostgreSQL

addConversation :: (MonadOrville m) => ConversationWrite -> m ConversationRead
addConversation = insertAndReturnEntity conversationTable

getConversationsByUserID :: (MonadOrville m) => UserID -> m [ConversationRead]
getConversationsByUserID userID =
  findEntitiesBy conversationTable $
    where_ (fieldEquals conversationUserIDField (Just userID))

getConversationsByPublicID ::
  (MonadOrville m) =>
  ConversationPublicID ->
  m (Maybe ConversationRead)
getConversationsByPublicID convPublicId =
  findFirstEntityBy conversationTable $
    where_ (fieldEquals conversationPublicIDField convPublicId)

deleteConversation :: (MonadOrville m) => ConversationID -> m ()
deleteConversation = deleteEntity conversationTable

updateConversation :: MonadOrville m => ConversationID -> ConversationWrite -> m ()
updateConversation = updateEntity conversationTable

findSummaryByConvID :: (MonadOrville m) => ConversationID -> m (Maybe OldConvSummaryRead)
findSummaryByConvID convId =
  findFirstEntityBy oldConvSummaryTable $
    where_ (fieldEquals oldConvSummaryConversationIDField convId)

addSummary :: (MonadOrville m) => OldConvSummaryWrite -> m OldConvSummaryRead
addSummary = insertAndReturnEntity oldConvSummaryTable
