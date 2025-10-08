module Modulus.BE.LLM.Providers
  ( fetchAllProviders
  ) where

import Data.Either (fromRight)
import Modulus.Common.Openrouter (OpenRouterModel (modelId), getOpenRouterModelList)
import Modulus.Common.Types (ModelProviders (..))
import Modulus.Common.Utils (listAvailableOllamaModels)
import UnliftIO.Async (concurrently)

fetchAllProviders :: IO [ModelProviders]
fetchAllProviders = do
  (ollamaModels, openRouterModels) <-
    concurrently
      listAvailableOllamaModels
      (map modelId . take 5 . fromRight [] <$> getOpenRouterModelList)
  pure
    [ toOllamaModels ollamaModels
    , toOpenRouterModels openRouterModels
    , geminiProviderList
    ]
  where
    toOllamaModels ollamaModelList =
      ModelProviders
        { providerName = "ollama"
        , modelList = ollamaModelList
        , isApiFieldRequired = False
        }
    toOpenRouterModels orList =
      ModelProviders
        { providerName = "openrouter"
        , modelList = orList
        , isApiFieldRequired = True
        }
    geminiProviderList =
      ModelProviders
        { providerName = "gemini"
        , modelList =
            [ "gemini-2.5-flash-lite"
            , "gemini-2.5-flash"
            , "gemini-2.5-pro"
            , "gemini-2.5-flash-image-preview"
            ]
        , isApiFieldRequired = True
        }
