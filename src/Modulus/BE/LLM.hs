module Modulus.BE.LLM (
    getRetriver
  , systemTemplate
) where

import Langchain.DocumentLoader.PdfLoader 
import Langchain.Embeddings.Ollama 
import Control.Monad.Except 
import Langchain.VectorStore.InMemory 
import Langchain.Retriever.Core 
import Data.Text (Text)
import qualified Data.Text as T

systemTemplate :: Text
systemTemplate =
  T.unlines
    [ "Use the following pieces of context to answer the user's question."
    , "If you don't know the answer, just say that you don't know, don't try to make up an answer."
    , "ALWAYS return a \"SOURCES\" part in your answer."
    , "The \"SOURCES\" part should be a reference to the source of the document from which you got your answer."
    , "Example response:"
    , "```"
    , "The answer is foo."
    , "SOURCES: xyz"
    , "```"
    , "{context}"
    ]

getRetriver :: FilePath 
    -> IO (Either String (VectorStoreRetriever (InMemory OllamaEmbeddings)))
getRetriver fp = do 
  let sourcePdf = PdfLoader fp
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing

  -- Prepare documents and vector store
  result <- runExceptT $ do
    docs <- ExceptT $ load sourcePdf
    ExceptT $ fromDocuments ollamaEmbeddings docs

  case result of
    Left err -> do 
      putStrLn $ "Error loading documents: " <> err
      pure $ Left "something went wrong while creating retriever"
    Right vectorStore -> do
      let retriever = VectorStoreRetriever vectorStore
      pure $ Right retriever
