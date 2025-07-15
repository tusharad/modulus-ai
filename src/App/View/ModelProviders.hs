module App.View.ModelProviders
  ( ModelProviders (..)
  , Action (..)
  , renderProviderListView
  ) where

import App.Common.Types
import App.Effects.StateStore
import App.View.GenerateReply
import Control.Monad (forM_)
import Data.Text (Text)
import Effectful
import Web.Atomic.CSS
import Web.Hyperbole

data ModelProviders = ModelProviders Int
  deriving (Generic, ViewId)

instance (StateStoreEff :> es, IOE :> es) => HyperView ModelProviders es where
  data Action ModelProviders
    = SetProvider Provider
    | SetOllamaModel Text
    | SetOpenRouterModel Text
    | SetOpenRouterApiKey Text
    deriving (Generic, ViewAction)

  update = \case
    SetProvider p -> updateProvider p
    SetOllamaModel model -> updateOllamaModel model
    SetOpenRouterModel model -> updateOpenRouterModel model
    SetOpenRouterApiKey apiKey -> updateOpenRouterApiKey apiKey

updateProvider ::
  (StateStoreEff :> es, IOE :> es) =>
  Provider -> Eff es (View ModelProviders ())
updateProvider p = do
  modifyState $ \st -> st {providerInfo = p}
  ollamaModels <- getAvailableOllamaModels
  pure $ renderProviderListView p ollamaModels

updateOllamaModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text -> Eff es (View ModelProviders ())
updateOllamaModel model = do
  let p = OllamaProvider model
  modifyState $ \st -> st {providerInfo = p}
  ollamaModels <- getAvailableOllamaModels
  pure $ renderProviderListView p ollamaModels

updateOpenRouterModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text -> Eff es (View ModelProviders ())
updateOpenRouterModel model = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> OpenRouterProvider model ""
        OpenRouterProvider _ apiKey -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  ollamaModels <- getAvailableOllamaModels
  pure $ renderProviderListView newP ollamaModels

updateOpenRouterApiKey ::
  (StateStoreEff :> es, IOE :> es) => Text -> Eff es (View ModelProviders ())
updateOpenRouterApiKey apiKey = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" apiKey
        OpenRouterProvider model _ -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  ollamaModels <- getAvailableOllamaModels
  pure $ renderProviderListView newP ollamaModels

renderProviderListView :: Provider -> [Text] -> View ModelProviders ()
renderProviderListView providerInfo ollamaModels = do
  el ~ cls "top-bar-right" $ do
    text "Provider"
    dropdown SetProvider (== providerInfo) ~ cls "form-select model-select" $ do
      showOllamaProviderOption ollamaModels
      option (OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" "") "OpenRouter"
    case providerInfo of
      OllamaProvider modelName -> renderOllamaModelsView modelName ollamaModels
      OpenRouterProvider modelName _ -> renderOpenRouterModelsView modelName

showOllamaProviderOption ::
  ViewAction (Action id) =>
  [Text] -> View (Option Provider id) ()
showOllamaProviderOption [] = none
showOllamaProviderOption (firstModel : _) = option (OllamaProvider firstModel) "Ollama"

renderOllamaModelsView :: Text -> [Text] -> View ModelProviders ()
renderOllamaModelsView modelName ollamaModels = do
  text "Model"
  dropdown SetOllamaModel (== modelName) ~ cls "form-select model-select w-auto" $ do
    forM_ ollamaModels $ \model -> do
      option model (text model)

renderOpenRouterModelsView :: Text -> View ModelProviders ()
renderOpenRouterModelsView modelName = do
  text "Model"
  dropdown SetOpenRouterModel (== modelName) ~ cls "form-select model-select w-auto" $ do
    option "mistralai/mistral-small-3.2-24b-instruct:free" "Mistral"
    option "deepseek/deepseek-chat-v3-0324:free" "Deepseek"
  tag "input"
    ~ cls "form-control"
      @ att "type" "password"
      . placeholder "API Key"
      . onInput SetOpenRouterApiKey 1000
    $ none
