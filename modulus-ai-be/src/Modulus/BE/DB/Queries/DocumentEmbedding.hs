module Modulus.BE.DB.Queries.DocumentEmbedding
  ( addDocumentEmbedding
  , getDocumentEmbeddingsByAttachmentId
  ) where

import Modulus.BE.DB.Internal.Marshaller.DocumentEmbedding
  ( documentEmbeddingMessageAttachmentIDField
  )
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (documentEmbeddingTable)
import Orville.PostgreSQL

addDocumentEmbedding ::
  (MonadOrville m) =>
  DocumentEmbeddingWrite ->
  m DocumentEmbeddingRead
addDocumentEmbedding = insertAndReturnEntity documentEmbeddingTable

getDocumentEmbeddingsByAttachmentId ::
  (MonadOrville m) => MessageAttachmentID -> m [DocumentEmbeddingRead]
getDocumentEmbeddingsByAttachmentId attId =
  findEntitiesBy documentEmbeddingTable $
    where_ (fieldEquals documentEmbeddingMessageAttachmentIDField attId)
