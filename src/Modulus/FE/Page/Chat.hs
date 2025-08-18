module Modulus.FE.Page.Chat (page) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effectful
import Modulus.Common.Types (AuthTokens (..))
import Modulus.FE.Effects.StateStore (StateStore (..), StateStoreEff, getState)
import Modulus.FE.Utils
import Modulus.FE.View.ChatInputView (ChatInputView (ChatInputView), chatInputView)
import Modulus.FE.View.ChatView (ChatView (ChatView), GenerateReplyView, loadChatView)
import Modulus.FE.View.ModelProviderView (ModelProviders)
import Modulus.FE.View.NavbarView
import Modulus.FE.View.SidebarView
import Web.Atomic.CSS
import Web.Hyperbole

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
         , ChatView
         , ChatInputView
         , GenerateReplyView
         ]
    )
page mbPublicConvId = do
  mbAuthTokens <- lookupSession @AuthTokens
  st <- getState
  let publicConvID = fromMaybe "1" mbPublicConvId -- if convID is 1, means it's a new chat
  case mbAuthTokens of
    Nothing -> redirect loginUrl
    Just _ -> do
      pure $ do
        stylesheet "/style.css"
        el ~ cls "main-layout" $ do
          myHyper (SidebarView 1) loadSidebarView
          el ~ cls "sidebar-overlay" $ none
          tag "main" ~ cls "main-content" $ do
            hyper
              (NavbarView publicConvID)
              ( navbarView
                  (providerInfo st)
                  (availableOllamaModels st)
                  (availableORModels st)
              )
            myHyper (ChatView publicConvID) (loadChatView mbPublicConvId)
            el ~ cls "input-area" $ do
              myHyper (ChatInputView publicConvID) chatInputView
