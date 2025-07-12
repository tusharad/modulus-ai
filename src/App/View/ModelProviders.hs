module App.View.ModelProviders (
    ModelProviders (..)
  , Action (..)
  , renderProviderListView
) where

import App.Common.Types
import App.Effects.StateStore
import Web.Atomic.CSS
import App.View.GenerateReply
import Data.Text (Text)
import Effectful
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
  pure $ renderProviderListView p

updateOllamaModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text -> Eff es (View ModelProviders ())
updateOllamaModel model = do
  let p = OllamaProvider model
  modifyState $ \st -> st {providerInfo = p}
  pure $ renderProviderListView p

updateOpenRouterModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text -> Eff es (View ModelProviders ())
updateOpenRouterModel model = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> OpenRouterProvider model ""
        OpenRouterProvider _ apiKey -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  pure $ renderProviderListView newP

updateOpenRouterApiKey ::
  (StateStoreEff :> es, IOE :> es) => Text -> Eff es (View ModelProviders ())
updateOpenRouterApiKey apiKey = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" apiKey
        OpenRouterProvider model _ -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  pure $ renderProviderListView newP

renderProviderListView :: Provider -> View ModelProviders ()
renderProviderListView providerInfo = do
  el ~ cls "top-bar-right" $ do
    text "Provider"
    dropdown SetProvider (== providerInfo) ~ cls "form-select model-select" $ do
      option (OllamaProvider "gemma3") "Ollama"
      option (OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" "") "OpenRouter"
    case providerInfo of
      OllamaProvider modelName -> renderOllamaModelsView modelName
      OpenRouterProvider modelName _ -> renderOpenRouterModelsView modelName

renderOllamaModelsView :: Text -> View ModelProviders ()
renderOllamaModelsView modelName = do
  text "Model"
  dropdown SetOllamaModel (== modelName) ~ cls "form-select model-select w-auto" $ do
    option "gemma3" "gemma3"
    option "qwen3:0.6b" "qwen3:0.6b"

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
