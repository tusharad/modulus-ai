{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.LLM.Embeddings
  ( docToText
  , getRelevantContext
  , storeOllamaEmbeddingsForAttachment
  , storeDocsForEmbeddings
  ) where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.DocumentLoader.Core
import qualified Langchain.Embeddings.Gemini as Gemini
import Langchain.Embeddings.Ollama
import Langchain.Retriever.Core
  ( Retriever (_get_relevant_documents)
  , VectorStoreRetriever (VectorStoreRetriever)
  )
import Langchain.VectorStore.InMemory (InMemory (..), fromDocuments)
import Modulus.BE.Api.Types (LLMRespStreamBody (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.DocumentEmbedding (addDocumentEmbedding)
import Orville.PostgreSQL (MonadOrville)

mkRetriverOllama ::
  MonadOrville m =>
  [DocumentEmbeddingRead] ->
  m (Either String (VectorStoreRetriever (InMemory OllamaEmbeddings)))
mkRetriverOllama docEmbedList = do
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing
  let st = docEmbedToStore 0 docEmbedList
  let vectorStore =
        InMemory
          { embeddingModel = ollamaEmbeddings
          , store = Map.fromList st
          }
  liftIO $ putStrLn "vectorStore created"
  let retriever = VectorStoreRetriever vectorStore
  pure $ Right retriever

docEmbedToStore :: Int64 -> [DocumentEmbeddingRead] -> [(Int64, (Document, [Float]))]
docEmbedToStore _ [] = []
docEmbedToStore idx (docEmbed : xs) =
  let d = Document (documentEmbeddingDocumentContent docEmbed) Map.empty
      e = map realToFrac $ NE.toList $ documentEmbeddingEmbedding docEmbed
   in (idx, (d, e)) : docEmbedToStore (idx + 1) xs

storeDocsForEmbeddings ::
  MonadOrville m =>
  LLMRespStreamBody ->
  MessageAttachmentID ->
  [Document] ->
  m (Either String [DocumentEmbeddingRead])
storeDocsForEmbeddings respStreamBody msgAttachID docs = do
  runExceptT $ do
    case provider respStreamBody of
      "ollama" -> ExceptT $ storeOllamaEmbeddingsForAttachment msgAttachID docs
      "gemini" -> do
        case apiKey respStreamBody of
          Nothing -> ExceptT $ pure $ Left "Please provide API Key for Gemini embeddings"
          Just aKey -> ExceptT $ storeGeminiEmbeddingsForAttachment msgAttachID docs aKey
      unknownProvider ->
        ExceptT . pure . Left $
          "Unsupported provider for embeddings: " <> T.unpack unknownProvider

-- Create embeddings using Ollama for provided docs and persist them for an attachment
storeOllamaEmbeddingsForAttachment ::
  MonadOrville m =>
  MessageAttachmentID ->
  [Document] ->
  m (Either String [DocumentEmbeddingRead])
storeOllamaEmbeddingsForAttachment attachmentId docs = do
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing
  eVecStore <- liftIO $ fromDocuments ollamaEmbeddings docs
  case eVecStore of
    Left err -> pure $ Left $ "Error generating embeddings: " <> err
    Right vectorStore -> do
      let docEmbedList = map snd $ Map.toList $ store vectorStore
      Right <$> mapM (insertDocEmbed attachmentId) docEmbedList

storeGeminiEmbeddingsForAttachment ::
  MonadOrville m =>
  MessageAttachmentID ->
  [Document] ->
  Text ->
  m (Either String [DocumentEmbeddingRead])
storeGeminiEmbeddingsForAttachment attachmentId docs aKey = do
  let geminiEmbeddings =
        Gemini.defaultGeminiEmbeddings
          { Gemini.apiKey = aKey
          , Gemini.dimensions = Just 768
          }
  eVecStore <- liftIO $ fromDocuments geminiEmbeddings docs
  case eVecStore of
    Left err -> do
      pure $ Left $ "Error loading documents: " <> err
    Right vectorStore -> do
      liftIO $ putStrLn "vector store for storing embedding created"
      let docEmbedList = map snd $ Map.toList $ store vectorStore
      Right <$> mapM (insertDocEmbed attachmentId) docEmbedList

insertDocEmbed ::
  MonadOrville m =>
  MessageAttachmentID ->
  (Document, [Float]) ->
  m DocumentEmbeddingRead
insertDocEmbed attId (d, e) = do
  let docEmbed =
        DocumentEmbedding
          { documentEmbeddingID = ()
          , documentEmbeddingMessageAttachmentID = attId
          , documentEmbeddingDocumentContent = pageContent d
          , documentEmbeddingEmbedding = NE.fromList $ map realToFrac e
          }
  addDocumentEmbedding docEmbed

mkRetriverGemini ::
  MonadOrville m =>
  [DocumentEmbeddingRead] ->
  Text ->
  m (Either String (VectorStoreRetriever (InMemory Gemini.GeminiEmbeddings)))
mkRetriverGemini docEmbedList aKey = do
  let geminiEmbeddings =
        Gemini.defaultGeminiEmbeddings
          { Gemini.apiKey = aKey
          , Gemini.dimensions = Just 768
          }
  let st = docEmbedToStore 0 docEmbedList
  let vectorStore =
        InMemory
          { embeddingModel = geminiEmbeddings
          , store = Map.fromList st
          }
  let retriever = VectorStoreRetriever vectorStore
  pure $ Right retriever

getRelevantContext ::
  MonadOrville m =>
  [DocumentEmbeddingRead] ->
  LLMRespStreamBody ->
  Text ->
  m (Either String Text)
getRelevantContext docsEmbedList LLMRespStreamBody {..} userQuery = do
  runExceptT $ do
    case provider of
      "ollama" -> do
        retriever <- ExceptT $ mkRetriverOllama docsEmbedList
        ExceptT $ liftIO $ fmap docToText <$> _get_relevant_documents retriever userQuery
      "gemini" -> do
        case apiKey of
          Nothing -> pure "missing api key for gemini"
          Just aKey -> do
            liftIO $ putStrLn "creating retriver..."
            retriever <- ExceptT $ mkRetriverGemini docsEmbedList aKey
            liftIO $ putStrLn "retriver created!"
            ExceptT $ liftIO $ fmap docToText <$> _get_relevant_documents retriever userQuery
      _ -> pure "Unsupported provider for embeddings. Use either Ollama or gemini"

docToText :: [Document] -> Text
docToText =
  mconcat
    . map
      (\doc -> pageContent doc <> T.pack (show $ metadata doc))
