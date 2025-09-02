{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.LLM.Embeddings
  ( docToText
  , getRelevantContext
  ) where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.DocumentLoader.Core
import qualified Langchain.Embeddings.Gemini as Gemini
import Langchain.Embeddings.Ollama
import Langchain.Retriever.Core
  ( Retriever (_get_relevant_documents)
  , VectorStoreRetriever (VectorStoreRetriever)
  )
import Langchain.VectorStore.InMemory (InMemory, fromDocuments)
import Modulus.BE.Api.Types (LLMRespStreamBody (..))

mkRetriverOllama ::
  [Document] -> IO (Either String (VectorStoreRetriever (InMemory OllamaEmbeddings)))
mkRetriverOllama docs = do
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing
  eVecStore <- fromDocuments ollamaEmbeddings docs
  case eVecStore of
    Left err -> do
      pure $ Left $ "Error loading documents: " <> err
    Right vectorStore -> do
      let retriever = VectorStoreRetriever vectorStore
      pure $ Right retriever

mkRetriverGemini ::
  [Document] -> Text -> IO (Either String (VectorStoreRetriever (InMemory Gemini.GeminiEmbeddings)))
mkRetriverGemini docs aKey = do
  let geminiEmbeddings =
        Gemini.defaultGeminiEmbeddings
          { Gemini.apiKey = aKey
          }
  eVecStore <- fromDocuments geminiEmbeddings docs
  case eVecStore of
    Left err -> do
      pure $ Left $ "Error loading documents: " <> err
    Right vectorStore -> do
      let retriever = VectorStoreRetriever vectorStore
      pure $ Right retriever

getRelevantContext :: [Document] -> LLMRespStreamBody -> Text -> IO (Either String Text)
getRelevantContext docs LLMRespStreamBody {..} userQuery = do
  runExceptT $ do
    case provider of
      "ollama" -> do
        retriever <- ExceptT $ mkRetriverOllama docs
        ExceptT $ fmap docToText <$> _get_relevant_documents retriever userQuery
      "gemini" -> do
        case apiKey of
          Nothing -> pure "missing api key for gemini"
          Just aKey -> do
            retriever <- ExceptT $ mkRetriverGemini docs aKey
            ExceptT $ fmap docToText <$> _get_relevant_documents retriever userQuery
      _ -> pure "Unsupported provider for embeddings. Use either Ollama or gemini"

docToText :: [Document] -> Text
docToText =
  mconcat
    . map
      (\doc -> pageContent doc <> T.pack (show $ metadata doc))
