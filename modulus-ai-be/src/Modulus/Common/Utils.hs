{-# LANGUAGE ScopedTypeVariables #-}

module Modulus.Common.Utils
  ( listAvailableOllamaModels
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Ollama

listAvailableOllamaModels :: IO [Text]
listAvailableOllamaModels = do
  eVersion <- Ollama.getVersion
  case eVersion of
    Left _ -> do
      putStrLn "Ollama not seems to be installed."
      pure []
    Right (Ollama.Version v) -> do
      putStrLn $ "Ollama version: " <> T.unpack v
      eModelInfo <- Ollama.list Nothing
      case eModelInfo of
        Left err -> print err >> pure []
        Right (Ollama.Models modelInfoList) -> do
          let modelNames = map Ollama.name modelInfoList
          pure modelNames
