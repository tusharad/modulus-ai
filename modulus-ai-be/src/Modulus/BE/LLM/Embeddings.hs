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
import Control.Monad.Reader (asks)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.DocumentLoader.Core
import qualified Langchain.Embeddings.Gemini as Gemini
import Langchain.Embeddings.Ollama
import Langchain.Error (LangchainError, fromString, toString)
import Langchain.Retriever.Core (Retriever (_get_relevant_documents))
import Langchain.VectorStore.InMemory (InMemory (..), fromDocuments)
import Modulus.BE.Api.Types (LLMRespStreamBody (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.DocumentEmbedding (addDocumentEmbedding)
import Modulus.BE.LLM.PGVectorRetriever (PGVectorRetriever (PGVectorRetriever))
import Modulus.BE.Monad.AppM (AppM)
import Modulus.Common.Types (AppConfig (configOrvilleState))
import Orville.PostgreSQL (MonadOrville)

storeDocsForEmbeddings ::
  MonadOrville m =>
  LLMRespStreamBody ->
  MessageAttachmentID ->
  [Document] ->
  m (Either LangchainError ())
storeDocsForEmbeddings respStreamBody msgAttachID docs = do
  runExceptT $ do
    case provider respStreamBody of
      "ollama" -> ExceptT $ storeOllamaEmbeddingsForAttachment msgAttachID docs
      "gemini" -> do
        case apiKey respStreamBody of
          Nothing -> ExceptT . pure . Left $ fromString "Please provide API Key for Gemini embeddings"
          Just aKey -> ExceptT $ storeGeminiEmbeddingsForAttachment msgAttachID docs aKey
      unknownProvider ->
        ExceptT
          . pure
          . Left
          . fromString
          $ "Unsupported provider for embeddings: " <> T.unpack unknownProvider

-- Create embeddings using Ollama for provided docs and persist them for an attachment
storeOllamaEmbeddingsForAttachment ::
  MonadOrville m =>
  MessageAttachmentID ->
  [Document] ->
  m (Either LangchainError ())
storeOllamaEmbeddingsForAttachment attachmentId docs = do
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing
  eVecStore <- liftIO $ fromDocuments ollamaEmbeddings docs
  case eVecStore of
    Left err -> pure $ Left $ fromString ("Error generating embeddings: " <> toString err)
    Right vectorStore -> do
      let docEmbedList = map snd $ Map.toList $ store vectorStore
      Right <$> mapM_ (insertDocEmbed attachmentId) docEmbedList

storeGeminiEmbeddingsForAttachment ::
  MonadOrville m =>
  MessageAttachmentID ->
  [Document] ->
  Text ->
  m (Either LangchainError ())
storeGeminiEmbeddingsForAttachment attachmentId docs aKey = do
  let geminiEmbeddings =
        Gemini.defaultGeminiEmbeddings
          { Gemini.apiKey = aKey
          , Gemini.dimensions = Just 768
          }
  eVecStore <- liftIO $ fromDocuments geminiEmbeddings docs
  case eVecStore of
    Left err -> do
      pure $ Left $ fromString $ "Error loading documents: " <> toString err
    Right vectorStore -> do
      liftIO $ putStrLn "vector store for storing embedding created"
      let docEmbedList = map snd $ Map.toList $ store vectorStore
      Right <$> mapM_ (insertDocEmbed attachmentId) docEmbedList

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

getRelevantContext ::
  LLMRespStreamBody ->
  Text ->
  AppM (Either LangchainError Text)
getRelevantContext LLMRespStreamBody {..} userQuery = do
  runExceptT $ do
    case provider of
      "ollama" -> do
        let ollamaEmbeddings =
              OllamaEmbeddings
                "nomic-embed-text:latest"
                Nothing
                Nothing
                Nothing
        orState <- asks configOrvilleState
        let ret = PGVectorRetriever ollamaEmbeddings orState
        ExceptT $ liftIO $ fmap docToText <$> _get_relevant_documents ret userQuery
      "gemini" -> do
        case apiKey of
          Nothing -> pure "missing api key for gemini"
          Just aKey -> do
            liftIO $ putStrLn "creating retriver..."
            let geminiEmbeddings =
                  Gemini.defaultGeminiEmbeddings
                    { Gemini.apiKey = aKey
                    , Gemini.dimensions = Just 768
                    }
            orState <- asks configOrvilleState
            let ret = PGVectorRetriever geminiEmbeddings orState
            liftIO $ putStrLn "retriver created!"
            ExceptT $ liftIO $ fmap docToText <$> _get_relevant_documents ret userQuery
      _ -> pure "Unsupported provider for embeddings. Use either Ollama or gemini"

docToText :: [Document] -> Text
docToText =
  mconcat
    . map
      (\doc -> pageContent doc <> T.pack (show $ metadata doc))
