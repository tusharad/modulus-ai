module Modulus.FE.View.NavbarView
  ( navbarView
  , NavbarView (..)
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Effectful
import Modulus.FE.Effects.StateStore
import Web.Atomic.CSS
import Web.Hyperbole

newtype NavbarView = NavbarView Text
  deriving (Generic, ViewId)

instance (StateStoreEff :> es, IOE :> es) => HyperView NavbarView es where
  data Action NavbarView
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
  Provider ->
  Eff es (View NavbarView ())
updateProvider p = do
  modifyState $ \st -> st {providerInfo = p}
  ollamaModels <- getAvailableOllamaModels
  navbarView p ollamaModels <$> getAvailableORModels

updateOllamaModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text ->
  Eff es (View NavbarView ())
updateOllamaModel model = do
  let p = OllamaProvider model
  modifyState $ \st -> st {providerInfo = p}
  ollamaModels <- getAvailableOllamaModels
  navbarView p ollamaModels <$> getAvailableORModels

updateOpenRouterModel ::
  (StateStoreEff :> es, IOE :> es) =>
  Text ->
  Eff es (View NavbarView ())
updateOpenRouterModel model = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ -> OpenRouterProvider model ""
        OpenRouterProvider _ apiKey -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  ollamaModels <- getAvailableOllamaModels
  navbarView newP ollamaModels <$> getAvailableORModels

updateOpenRouterApiKey ::
  (StateStoreEff :> es, IOE :> es) => Text -> Eff es (View NavbarView ())
updateOpenRouterApiKey apiKey = do
  st <- getState
  let newP = case providerInfo st of
        OllamaProvider _ ->
          OpenRouterProvider "deepseek/deepseek-chat-v3-0324:free" apiKey
        -- TODO: use first model from list
        OpenRouterProvider model _ -> OpenRouterProvider model apiKey
  modifyState $ \s -> s {providerInfo = newP}
  ollamaModels <- getAvailableOllamaModels
  navbarView newP ollamaModels <$> getAvailableORModels

navbarView :: Provider -> [Text] -> [Text] -> View NavbarView ()
navbarView p ollamaList orList =
  tag "nav" ~ cls "navbar navbar-expand-lg border-bottom" $ do
    tag "div" ~ cls "container-fluid" $ do
      tag "a"
        ~ cls "navbar-brand fw-bold"
          @ att "href" "#"
        $ text "AI Chatbot"
      tag "button"
        ~ cls "navbar-toggler navbarToggleBtn"
          @ att "type" "button"
          . att "onClick" "toggleNavbar()"
          . att "aria-expanded" "false"
          . att "aria-label" "Toggle navigation"
        $ tag "span" ~ cls "navbar-toggler-icon"
        $ none
      tag "div" ~ cls "collapse navbar-collapse navbarToggleDiv" $
        tag "ul" ~ cls "navbar-nav ms-auto align-items-center" $ do
          renderProviderListView p ollamaList orList
          tag "li" ~ cls "nav-item dropdown ms-3" $ do
            tag "a"
              ~ cls "nav-link p-0"
                @ att "href" "#"
                . att "role" "button"
                . att "data-bs-toggle" "dropdown"
                . att "aria-expanded" "false"
              $ tag "img"
                ~ cls "rounded-circle"
                  @ att "src" "https://placehold.co/40x40/8b5cf6/white?text=A"
                  . att "alt" "Profile"
              $ none
            tag "ul" ~ cls "dropdown-menu dropdown-menu-end" $ do
              tag "li" $ tag "a" ~ cls "dropdown-item" @ att "href" "#" $ do
                tag "i" ~ cls "bi bi-gear me-2" $ none
                text "Settings"
              tag "li" $ tag "hr" ~ cls "dropdown-divider" $ none
              tag "li" $ tag "a" ~ cls "dropdown-item" @ att "href" "#" $ do
                tag "i" ~ cls "bi bi-box-arrow-right me-2" $ none
                text "Logout"

showORProviderOption :: [Text] -> View NavbarView ()
showORProviderOption [] = none
showORProviderOption (firstModel : _) =
  tag "li" $
    button (SetProvider (OpenRouterProvider firstModel "")) ~ cls "dropdown-item" $
      text "OpenRouter"

showOllamaProviderOption :: [Text] -> View NavbarView ()
showOllamaProviderOption [] = none
showOllamaProviderOption (firstModel : _) =
  tag "li" $
    button (SetProvider (OllamaProvider firstModel)) ~ cls "dropdown-item" $
      text "Ollama"

renderProviderListView :: Provider -> [Text] -> [Text] -> View NavbarView ()
renderProviderListView providerInfo ollamaModels orModels = do
  tag "li" ~ cls "nav-item dropdown" $ do
    tag "a"
      ~ cls "nav-link dropdown-toggle"
        @ att "href" "#"
        . att "role" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
      $ do
        tag "i" ~ cls "bi bi-cpu me-1" $ none
        text $ case providerInfo of
          OllamaProvider _ -> "Ollama"
          OpenRouterProvider _ _ -> "OpenRouter"
    tag "ul" ~ cls "dropdown-menu dropdown-menu-end" $ do
      showOllamaProviderOption ollamaModels
      showORProviderOption orModels
  case providerInfo of
    OllamaProvider modelName -> renderOllamaModelsView modelName ollamaModels
    OpenRouterProvider modelName apiKey -> renderOpenRouterModelsView apiKey modelName orModels

renderOllamaModelsView :: Text -> [Text] -> View NavbarView ()
renderOllamaModelsView currentlySelectedModel ollamaModels = do
  tag "li" ~ cls "nav-item dropdown mx-2" $ do
    tag "a"
      ~ cls "nav-link dropdown-toggle"
        @ att "href" "#"
        . att "role" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
      $ do
        tag "i" ~ cls "bi bi-gem me-1" $ none
        text currentlySelectedModel
    tag "ul" ~ cls "dropdown-menu dropdown-menu-end" $ do
      forM_ ollamaModels $ \model -> do
        tag "li" $ button (SetOllamaModel model) ~ cls "dropdown-item" $ text model
  tag "li" ~ cls "nav-item dropdown" $ do
    tag "a"
      ~ cls "nav-link"
        @ att "href" "#"
        . att "role" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
      $ tag "i" ~ cls "bi bi-key fs-5"
      $ none

renderOpenRouterModelsView :: Text -> Text -> [Text] -> View NavbarView ()
renderOpenRouterModelsView _ _ [] = none
renderOpenRouterModelsView apiKey currentlySelectedModel orModels = do
  tag "li" ~ cls "nav-item dropdown mx-2" $ do
    tag "a"
      ~ cls "nav-link dropdown-toggle"
        @ att "href" "#"
        . att "role" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
      $ do
        tag "i" ~ cls "bi bi-gem me-1" $ none
        text currentlySelectedModel
    tag "ul" ~ cls "dropdown-menu dropdown-menu-end" $ do
      forM_ orModels $ \model -> do
        tag "li" $ button (SetOpenRouterModel model) ~ cls "dropdown-item" $ text model
  tag "li" ~ cls "nav-item dropdown" $ do
    tag "a"
      ~ cls "nav-link"
        @ att "href" "#"
        . att "role" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
      $ tag "i" ~ cls "bi bi-key fs-5"
      $ none
    tag "div"
      ~ cls "dropdown-menu dropdown-menu-end p-2"
        @ att "style" "width: 250px;"
      $ do
        tag "div" ~ cls "mb-2" $ do
          tag "label"
            ~ cls "form-label small"
              @ att "for" "apiKey"
            $ text "API Key"
          tag "input"
            ~ cls "form-control form-control-sm"
              @ att "type" "password"
              . onInput SetOpenRouterApiKey 1000
              . value apiKey
              . att "placeholder" "Enter your API key"
            $ none
