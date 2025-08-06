{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.Message
  ( messageAttachmentIDField
  , messageAttachmentMessageIDField
  , messageAttachmentOrganizationIDField
  , messageAttachmentFileNameField
  , messageAttachmentFileTypeField
  , messageAttachmentFileSizeBytesField
  , messageAttachmentStoragePathField
  , messageAttachmentCreatedAtField
  , messageAttachmentMarshaller
  ) where

import Control.Lens.Internal.CTypes (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.Organization (organizationCreatedAtField)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Message Attachment Fields
messageAttachmentIDField :: FieldDefinition NotNull MessageAttachmentID
messageAttachmentIDField = coerceField $ bigSerialField "id"

messageAttachmentMessageIDField :: FieldDefinition NotNull ChatMessageID
messageAttachmentMessageIDField = coerceField $ bigIntegerField "message_id"

messageAttachmentOrganizationIDField :: FieldDefinition NotNull OrganizationID
messageAttachmentOrganizationIDField = coerceField $ uuidField "organization_id"

messageAttachmentFileNameField :: FieldDefinition NotNull Text
messageAttachmentFileNameField = unboundedTextField "file_name"

messageAttachmentFileTypeField :: FieldDefinition NotNull Text
messageAttachmentFileTypeField = unboundedTextField "file_type"

messageAttachmentFileSizeBytesField :: FieldDefinition NotNull Int64
messageAttachmentFileSizeBytesField = bigIntegerField "file_size_bytes"

messageAttachmentStoragePathField :: FieldDefinition NotNull Text
messageAttachmentStoragePathField = unboundedTextField "storage_path"

messageAttachmentCreatedAtField :: FieldDefinition NotNull UTCTime
messageAttachmentCreatedAtField = organizationCreatedAtField

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
      (\MessageAttachment {..} -> messageAttachmentOrganizationID)
      messageAttachmentOrganizationIDField
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
