module Modulus.BE.DB.Queries.ChatMessage
  ( addChatMessage
  , getChatMessagesByConvID
  , addMsgAttachment
  , findChatMessageWithAttachments
  , getChatMessagesWithAttachmentsByConvID
  ) where

import Control.Monad (forM)
import Modulus.BE.DB.Internal.Marshaller.ChatMessage
import Modulus.BE.DB.Internal.Marshaller.MessageAttachment
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (chatMessageTable, messageAttachmentTable)
import Orville.PostgreSQL

addChatMessage :: (MonadOrville m) => ChatMessageWrite -> m ChatMessageRead
addChatMessage = insertAndReturnEntity chatMessageTable

getChatMessagesByConvID :: (MonadOrville m) => ConversationID -> m [ChatMessageRead]
getChatMessagesByConvID convID =
  findEntitiesBy chatMessageTable $
    where_ (fieldEquals chatMessageConversationIDField convID)
      <> orderBy (orderByField chatMessageIDField ascendingOrder)

addMsgAttachment ::
  (MonadOrville m) => MessageAttachmentWrite -> m MessageAttachmentRead
addMsgAttachment = insertAndReturnEntity messageAttachmentTable

findChatMessageWithAttachments ::
  (MonadOrville m) =>
  ChatMessageID ->
  m (Maybe ChatMessageWithAttachments)
findChatMessageWithAttachments msgId = do
  mChat <- findEntity chatMessageTable msgId
  case mChat of
    Nothing -> pure Nothing
    Just chat -> do
      atts <-
        findEntitiesBy messageAttachmentTable $
          where_ (fieldEquals messageAttachmentMessageIDField msgId)
      pure $ Just (ChatMessageWithAttachments chat atts)

getChatMessagesWithAttachmentsByConvID ::
  (MonadOrville m) =>
  ConversationID ->
  m [ChatMessageWithAttachments]
getChatMessagesWithAttachmentsByConvID convID = do
  msgs <- getChatMessagesByConvID convID
  -- For each message, load attachments
  forM msgs $ \chat -> do
    atts <-
      findEntitiesBy messageAttachmentTable $
        where_ (fieldEquals messageAttachmentMessageIDField (chatMessageID chat))
    pure (ChatMessageWithAttachments chat atts)
