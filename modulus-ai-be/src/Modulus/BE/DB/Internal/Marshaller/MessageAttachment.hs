{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.MessageAttachment
  ( messageAttachmentIDField
  , messageAttachmentMessageIDField
  , messageAttachmentFileNameField
  , messageAttachmentFileTypeField
  , messageAttachmentFileSizeBytesField
  , messageAttachmentStoragePathField
  , messageAttachmentCreatedAtField
  , messageAttachmentMarshaller
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.User (userCreatedAtField)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Message Attachment Fields
messageAttachmentIDField :: FieldDefinition NotNull MessageAttachmentID
messageAttachmentIDField = coerceField $ bigSerialField "id"

messageAttachmentMessageIDField :: FieldDefinition NotNull ChatMessageID
messageAttachmentMessageIDField = coerceField $ bigIntegerField "message_id"

messageAttachmentFileNameField :: FieldDefinition NotNull Text
messageAttachmentFileNameField = unboundedTextField "file_name"

messageAttachmentFileTypeField :: FieldDefinition NotNull Text
messageAttachmentFileTypeField = unboundedTextField "file_type"

messageAttachmentFileSizeBytesField :: FieldDefinition NotNull Int64
messageAttachmentFileSizeBytesField = bigIntegerField "file_size_bytes"

messageAttachmentStoragePathField :: FieldDefinition NotNull Text
messageAttachmentStoragePathField = unboundedTextField "storage_path"

messageAttachmentCreatedAtField :: FieldDefinition NotNull UTCTime
messageAttachmentCreatedAtField = userCreatedAtField

-- Message Attachment Marshaller
messageAttachmentMarshaller ::
  SqlMarshaller MessageAttachmentWrite MessageAttachmentRead
messageAttachmentMarshaller =
  MessageAttachment
    <$> marshallReadOnly
      ( marshallField
          (\MessageAttachment {..} -> messageAttachmentID)
          messageAttachmentIDField
      )
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentMessageID)
      messageAttachmentMessageIDField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileName)
      messageAttachmentFileNameField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileType)
      messageAttachmentFileTypeField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileSizeBytes)
      messageAttachmentFileSizeBytesField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentStoragePath)
      messageAttachmentStoragePathField
    <*> marshallReadOnly
      ( marshallField
          (\MessageAttachment {..} -> messageAttachmentCreatedAt)
          messageAttachmentCreatedAtField
      )
