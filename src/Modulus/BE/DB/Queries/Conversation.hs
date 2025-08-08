module Modulus.BE.DB.Queries.Conversation (
    addConversation
  , getConversationsByUserID 
) where

import Orville.PostgreSQL 
import Modulus.BE.DB.Internal.Model 
import Modulus.BE.DB.Internal.Table (conversationTable)
import Modulus.BE.DB.Internal.Marshaller.Conversation (conversationUserIDField)

addConversation :: MonadOrville m => ConversationWrite -> m ConversationRead
addConversation = insertAndReturnEntity conversationTable

getConversationsByUserID :: MonadOrville m => UserID -> m [ConversationRead]
getConversationsByUserID userID = 
    findEntitiesBy conversationTable $ 
        where_ (fieldEquals conversationUserIDField (Just userID))
