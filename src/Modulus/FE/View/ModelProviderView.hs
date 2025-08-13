module Modulus.FE.View.ModelProviderView
  ( ModelProviders (..)
  , Action (..)
  , renderProviderListView
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful
import Modulus.FE.Effects.StateStore
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
  renderProviderListView p ollamaModels <$> getAvailableORModels

updateOllamaModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text -> Eff es (View ModelProviders ())
updateOllamaModel model = do
  let p = OllamaProvider model
  modifyState $ \st -> st {providerInfo = p}
  ollamaModels <- getAvailableOllamaModels
  renderProviderListView p ollamaModels <$> getAvailableORModels

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
  renderProviderListView newP ollamaModels <$> getAvailableORModels

updateOpenRouterApiKey ::
  (StateStoreEff :> es, IOE :> es) => Text -> Eff es (View ModelProviders ())
updateOpenRouterApiKey apiKey = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> 
            OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" apiKey
        OpenRouterProvider model _ -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  ollamaModels <- getAvailableOllamaModels
  renderProviderListView newP ollamaModels <$> getAvailableORModels

renderProviderListView :: Provider -> [Text] -> [Text] -> View ModelProviders ()
renderProviderListView providerInfo ollamaModels orModels = do
  dropdown SetProvider (== providerInfo)
    ~ cls "dropdown"
    $ do
      showOllamaProviderOption ollamaModels
      showORProviderOption orModels
  case providerInfo of
    OllamaProvider modelName -> renderOllamaModelsView modelName ollamaModels
    OpenRouterProvider modelName _ -> renderOpenRouterModelsView modelName orModels

showORProviderOption ::
  ViewAction (Action id) =>
  [Text] -> View (Option Provider id) ()
showORProviderOption [] = none
showORProviderOption (firstModel : _) =
  option (OpenRouterProvider firstModel "")
    ~ cls "btn btn-sm btn-outline-secondary"
    $ "OpenRouter"

showOllamaProviderOption ::
  ViewAction (Action id) =>
  [Text] -> View (Option Provider id) ()
showOllamaProviderOption [] = none
showOllamaProviderOption (firstModel : _) =
  option (OllamaProvider firstModel)
    ~ cls "btn btn-sm btn-outline-secondary"
    $ "Ollama"

renderOllamaModelsView :: Text -> [Text] -> View ModelProviders ()
renderOllamaModelsView modelName ollamaModels = do
  dropdown SetOllamaModel (== modelName) ~ cls "dropdown" $ do
    forM_ ollamaModels $ \model -> do
      option model ~ cls "btn btn-sm btn-outline-secondary dropdown-toggle" $ text model

renderOpenRouterModelsView :: Text -> [Text] -> View ModelProviders ()
renderOpenRouterModelsView modelName orModels = do
  dropdown SetOpenRouterModel (== modelName) ~ cls "dropdown" $ do
    forM_ orModels $ \model -> do
      option model ~ cls "btn btn-sm btn-outline-secondary dropdown-toggle" $ text model
  tag "input"
    ~ cls "dropdown"
      @ att "type" "password"
      . placeholder "API Key"
      . onInput SetOpenRouterApiKey 1000
    $ none
