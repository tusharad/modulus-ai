module Modulus.BE.LLM.PGVectorRetriever
  ( PGVectorRetriever (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core
import Langchain.Retriever.Core
import Langchain.Runnable.Core (Runnable (RunnableInput, RunnableOutput, invoke))
import Modulus.BE.DB.Internal.Model (DocumentEmbedding (documentEmbeddingDocumentContent))
import Modulus.BE.DB.Queries.DocumentEmbedding (getNearestDocuments)
import Orville.PostgreSQL (runOrvilleWithState)
import qualified Orville.PostgreSQL.OrvilleState as OrvilleState

data Embeddings a => PGVectorRetriever a = PGVectorRetriever
  { embeddingModel :: a
  , orvilleState :: OrvilleState.OrvilleState
  }

instance Embeddings a => Retriever (PGVectorRetriever a) where
  _get_relevant_documents (PGVectorRetriever embModel orvilleSt) query = do
    eQueryEmbeddings <- embedQuery embModel query
    case eQueryEmbeddings of
      Left err -> pure $ Left err
      Right queryEmbeddings -> do
        docEmbedList <-
          runOrvilleWithState
            orvilleSt
            (getNearestDocuments $ map realToFrac queryEmbeddings)
        pure $
          Right $
            map
              ( \docEmbed ->
                  Document
                    (documentEmbeddingDocumentContent docEmbed)
                    Map.empty
              )
              docEmbedList

instance Embeddings a => Runnable (PGVectorRetriever a) where
  type RunnableInput (PGVectorRetriever a) = Text
  type RunnableOutput (PGVectorRetriever a) = [Document]

  invoke = _get_relevant_documents
