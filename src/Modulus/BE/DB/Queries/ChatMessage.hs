module Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , getChatMessagesByConvID
  ) where

import Modulus.BE.DB.Internal.Marshaller.ChatMessage (chatMessageConversationIDField)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (chatMessageTable)
import Orville.PostgreSQL

addChatMessage :: MonadOrville m => ChatMessageWrite -> m ChatMessageRead
addChatMessage = insertAndReturnEntity chatMessageTable

getChatMessagesByConvID :: MonadOrville m => ConversationID -> m [ChatMessageRead]
getChatMessagesByConvID convID =
  findEntitiesBy chatMessageTable $
    where_ (fieldEquals chatMessageConversationIDField convID)
