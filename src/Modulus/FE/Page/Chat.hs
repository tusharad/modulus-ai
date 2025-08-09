module Modulus.FE.Page.Chat (page) where

import Data.Text (Text)
import Effectful
import Modulus.Common.Types (AuthTokens (..))
import Modulus.FE.Utils (loginUrl)
import Modulus.FE.View.NavbarView
import Modulus.FE.View.SidebarView
import Web.Atomic.CSS
import Web.Hyperbole
import Modulus.FE.Effects.StateStore (getState, StateStore (..), StateStoreEff)
import Modulus.FE.View.ModelProviderView (ModelProviders)

--- Page
page ::
  (Hyperbole :> es, StateStoreEff :> es, IOE :> es) =>
  Maybe Text ->
  Eff
    es
    ( Page
        '[ SidebarView
         , NavbarView
         , ModelProviders
         ]
    )
page _ = do
  mbAuthTokens <- lookupSession @AuthTokens
  st <- getState
  case mbAuthTokens of
    Nothing -> redirect loginUrl
    Just _ -> do
      pure $ do
        stylesheet "/style.css"
        el ~ cls "main-layout" $ do
          hyper (SidebarView 1) loadSidebarView
          el ~ cls "sidebar-overlay" $ none
          tag "main" $ do
            hyper (NavbarView 1) (navbarView (providerInfo st) (availableOllamaModels st) (availableORModels st))
            el ~ cls "chat-window" $ do 
              el ~ cls "message ai-message" $ do 
                  text "ello! How can I help you today? I see you're interested in Rust and" 
                  el ~ cls "message-meta" $ "AI Assistant &middot; 4:47 PM"
              el ~ cls "message user-message" $ do 
                  text "Can you give me a basic example of using wasmtime in Rust to run a simple WAT functi" 
                  el ~ cls "message-meta" $ "AI Assistant &middot; 4:48 PM"
            el ~ cls "input-area" $ do 
              tag "form" $ do 
                  el ~ cls "input-wrapper" $ do 
                     tag "textarea" ~ cls "form-control" 
                                    @ att "placeholder" "Ask me anything..."
                                    . att "rows" "1" $ none
                     tag "button" ~ cls "btn btn-primary rounded-circle p-2" $ tag "i" ~ cls "bi bi-arrow-up" $ none
              el ~ cls "tool-options" $ do 
                tag "button" ~ cls "btn tool-btn" $ tag "i" ~ cls "bi bi-paperclip" $ none
                tag "button" ~ cls "btn tool-btn" $ do 
                    tag "i" ~ cls "bi bi-search me-1" $ none
                    text "Web search"
                tag "button" ~ cls "btn tool-btn" $ do 
                    tag "i" ~ cls "bi bi-lightbulb me-1" $ none
                    text "Thinking"
                tag "button" ~ cls "btn tool-btn" $ do 
                    tag "i" ~ cls "bi bi-wikipedia me-1" $ none
                    text "Wikipedia"
