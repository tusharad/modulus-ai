{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.Conversation
  ( -- * Conversation Marshallers
    conversationIDField
  , conversationPublicIDField
  , conversationUserIDField
  , conversationTitleField
  , conversationCreatedAtField
  , conversationUpdatedAtField
  , conversationMarshaller
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.User (userCreatedAtField, userUpdatedAtField)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils
import Orville.PostgreSQL

-- Conversation Fields
conversationIDField :: FieldDefinition NotNull ConversationID
conversationIDField = coerceField $ bigSerialField "id"

conversationPublicIDField :: FieldDefinition NotNull ConversationPublicID
conversationPublicIDField =
  coerceField $
    setDefaultValue genRandomUuidDefault $
      uuidField "public_id"

conversationUserIDField :: FieldDefinition Nullable (Maybe UserID)
conversationUserIDField = nullableField $ coerceField $ uuidField "user_id"

conversationTitleField :: FieldDefinition NotNull Text
conversationTitleField = unboundedTextField "title"

conversationCreatedAtField :: FieldDefinition NotNull UTCTime
conversationCreatedAtField = userCreatedAtField

conversationUpdatedAtField :: FieldDefinition NotNull UTCTime
conversationUpdatedAtField = userUpdatedAtField

-- Conversation Marshaller
conversationMarshaller :: SqlMarshaller ConversationWrite ConversationRead
conversationMarshaller =
  Conversation
    <$> marshallReadOnly
      (marshallField (\Conversation {..} -> conversationID) conversationIDField)
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationPublicID)
          conversationPublicIDField
      )
    <*> marshallField
      (\Conversation {..} -> conversationUserID)
      conversationUserIDField
    <*> marshallField (\Conversation {..} -> conversationTitle) conversationTitleField
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationCreatedAt)
          conversationCreatedAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationUpdatedAt)
          conversationUpdatedAtField
      )
