{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.DocumentEmbedding
  ( documentEmbeddingIDField
  , documentEmbeddingMessageAttachmentIDField
  , documentEmbeddingDocumentContentField
  , documentEmbeddingEmbeddingField
  , documentEmbeddingMarshaller
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Document Embedding Fields
documentEmbeddingIDField :: FieldDefinition NotNull DocumentEmbeddingID
documentEmbeddingIDField = coerceField $ serialField "document_id"

documentEmbeddingMessageAttachmentIDField ::
  FieldDefinition NotNull MessageAttachmentID
documentEmbeddingMessageAttachmentIDField =
  coerceField $
    bigIntegerField "message_attachment_id"

documentEmbeddingDocumentContentField :: FieldDefinition NotNull Text
documentEmbeddingDocumentContentField = unboundedTextField "document_content"

documentEmbeddingEmbeddingField :: FieldDefinition NotNull (NE.NonEmpty Double)
documentEmbeddingEmbeddingField = vectorFieldWithDimension "embedding" 768

-- Document Embedding Marshaller
documentEmbeddingMarshaller :: SqlMarshaller DocumentEmbeddingWrite DocumentEmbeddingRead
documentEmbeddingMarshaller =
  DocumentEmbedding
    <$> marshallReadOnly
      ( marshallField
          (\DocumentEmbedding {..} -> documentEmbeddingID)
          documentEmbeddingIDField
      )
    <*> marshallField
      (\DocumentEmbedding {..} -> documentEmbeddingMessageAttachmentID)
      documentEmbeddingMessageAttachmentIDField
    <*> marshallField
      (\DocumentEmbedding {..} -> documentEmbeddingDocumentContent)
      documentEmbeddingDocumentContentField
    <*> marshallField
      (\DocumentEmbedding {..} -> documentEmbeddingEmbedding)
      documentEmbeddingEmbeddingField
