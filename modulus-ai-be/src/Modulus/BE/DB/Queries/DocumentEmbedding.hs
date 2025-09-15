module Modulus.BE.DB.Queries.DocumentEmbedding
  ( addDocumentEmbedding
  , getDocumentEmbeddingsByAttachmentId
  , getNearestDocuments
  ) where

import qualified Data.List.NonEmpty as NE
import Modulus.BE.DB.Internal.Marshaller.DocumentEmbedding
  ( documentEmbeddingEmbeddingField
  , documentEmbeddingMessageAttachmentIDField
  )
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (documentEmbeddingTable)
import Orville.PostgreSQL
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.FieldDefinition as Expr
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

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

getNearestDocuments :: MonadOrville m => [Double] -> m [DocumentEmbeddingRead]
getNearestDocuments queryEmbeddings =
  findEntitiesBy documentEmbeddingTable $
    limit 5
      <> orderBy
        ( Expr.orderByValueExpression
            ( cosineDistance
                (Expr.fieldColumnReference documentEmbeddingEmbeddingField)
                (Expr.valueExpression $ SqlValue.toVector $ NE.fromList queryEmbeddings)
            )
            ascendingOrder
        )
