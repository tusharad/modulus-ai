{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.ChatMessage
  ( -- * Message Role Marshaller
    messageRoleField

    -- * Chat Message Marshallers
  , chatMessageIDField
  , chatMessagePublicIDField
  , chatMessageConversationIDField
  , chatMessageRoleField
  , chatMessageContentField
  , chatMessageModelUsedField
  , chatMessageCreatedAtField
  , chatMessageMarshaller
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils
import Orville.PostgreSQL
import Modulus.BE.DB.Internal.Marshaller.User (userCreatedAtField)

-- Chat Message Fields
chatMessageIDField :: FieldDefinition NotNull ChatMessageID
chatMessageIDField = coerceField $ bigSerialField "id"

chatMessagePublicIDField :: FieldDefinition NotNull ChatMessagePublicID
chatMessagePublicIDField =
  coerceField $
    setDefaultValue genRandomUuidDefault $
      uuidField "public_id"

chatMessageConversationIDField :: FieldDefinition NotNull ConversationID
chatMessageConversationIDField = coerceField $ bigIntegerField "conversation_id"


chatMessageRoleField :: FieldDefinition NotNull MessageRole
chatMessageRoleField = messageRoleField

chatMessageContentField :: FieldDefinition NotNull Text
chatMessageContentField = unboundedTextField "content"

chatMessageModelUsedField :: FieldDefinition Nullable (Maybe Text)
chatMessageModelUsedField = nullableField $ unboundedTextField "model_used"

chatMessageCreatedAtField :: FieldDefinition NotNull UTCTime
chatMessageCreatedAtField = userCreatedAtField

messageRoleSqlType :: SqlType MessageRole
messageRoleSqlType =
  tryConvertSqlType convertRoleToString convertStringToRole unboundedText
  where
    convertRoleToString :: MessageRole -> Text
    convertRoleToString MessageRoleUser = "user"
    convertRoleToString MessageRoleAssistant = "assistant"
    convertRoleToString MessageRoleSystem = "system"
    convertRoleToString MessageRoleTool = "tool"

    convertStringToRole :: Text -> Either String MessageRole
    convertStringToRole "user" = Right MessageRoleUser
    convertStringToRole "assistant" = Right MessageRoleAssistant
    convertStringToRole "system" = Right MessageRoleSystem
    convertStringToRole "tool" = Right MessageRoleTool
    convertStringToRole s = Left $ "Invalid MessageRole value: " ++ T.unpack s

messageRoleFieldFunc :: String -> FieldDefinition NotNull MessageRole
messageRoleFieldFunc = fieldOfType messageRoleSqlType

-- Message Role Field
messageRoleField :: FieldDefinition NotNull MessageRole
messageRoleField = messageRoleFieldFunc "role"

-- Chat Message Marshaller
chatMessageMarshaller :: SqlMarshaller ChatMessageWrite ChatMessageRead
chatMessageMarshaller =
  ChatMessage
    <$> marshallReadOnly
      (marshallField (\ChatMessage {..} -> chatMessageID) chatMessageIDField)
    <*> marshallReadOnly
      ( marshallField
          (\ChatMessage {..} -> chatMessagePublicID)
          chatMessagePublicIDField
      )
    <*> marshallField
      (\ChatMessage {..} -> chatMessageConversationID)
      chatMessageConversationIDField
    <*> marshallField (\ChatMessage {..} -> chatMessageRole) chatMessageRoleField
    <*> marshallField (\ChatMessage {..} -> chatMessageContent) chatMessageContentField
    <*> marshallField
      (\ChatMessage {..} -> chatMessageModelUsed)
      chatMessageModelUsedField
    <*> marshallReadOnly
      ( marshallField
          (\ChatMessage {..} -> chatMessageCreatedAt)
          chatMessageCreatedAtField
      )
