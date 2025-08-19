module Modulus.FE.View.SidebarView
  ( SidebarView (..)
  , loadSidebarView
  , chatDotsFilled
  , Action (..)
  ) where

import Control.Monad (forM_, void)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (IOE)
import Modulus.BE.Client.V1
import Modulus.BE.DB.Internal.Model
import Modulus.BE.Log (logDebug)
import Modulus.Common.Utils (runBE, runBEAuth)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Utils
import Web.Atomic.CSS
import Web.Hyperbole

newtype SidebarView = SidebarView Int
  deriving (Generic, ViewId)

instance (IOE :> es, AppConfigEff :> es) => HyperView SidebarView es where
  data Action SidebarView = LoadSidebar
    deriving (Generic, ViewAction)

  update LoadSidebar = do
    eConversationList <- runBEAuth getConversationsHandler
    case eConversationList of
      Left err -> pure $ errorView (T.pack $ show err)
      Right conversationLst -> do
        void . runBE $ logDebug $ "fetched list for sidebar : " <> T.pack (show conversationLst)
        pure $ sidebarView conversationLst

errorView :: Text -> View c ()
errorView err = tag "aside" $ do
  el ~ cls "sidebar-header d-flex justify-content-between align-items-center" $ do
    tag "h5" ~ cls "mb-0" $ "History"
  el ~ cls "history-list" $ do
    text err

loadSidebarView :: View SidebarView ()
loadSidebarView = do
  el @ onLoad LoadSidebar 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."

chatDotsFilled :: View c ()
chatDotsFilled = tag "i" ~ cls "bi bi-chat-dots-fill me-2" $ none

sidebarView :: [ConversationRead] -> View SidebarView ()
sidebarView convLst = do
  tag "div" ~ cls "d-flex flex-column flex-shrink-0 p-3 sidebar" @ att "id" "my-sidebar" $ do
    el
      ~ cls
        "d-flex align-items-center mb-3 mb-md-0 me-md-auto text-white text-decoration-none sidebar-brand"
        @ att "href" "/"
      $ do
        tag "i" ~ cls "bi bi-robot me-2 fs-4" $ none
        tag "span" ~ cls "fs-4 sidebar-brand-text" $ text "Chat History"
    tag "hr" none
    tag "ul" ~ cls "nav nav-pills flex-column mb-auto" $ do
      forM_ convLst $ \conv -> do
        tag "li" ~ cls "nav-item" $ do
          link (chatUrl $ conversationPublicID conv)
            ~ cls "nav-link active" -- TODO: get convID as arg, check if active or not
            $ do
              el ~ cls "d-flex align-items-center history-item-text" $ do
                tag "i" ~ cls "bi bi-chat-dots fs-5 me-2" $ none
                tag "span" ~ cls "sidebar-text" $ do
                  text $ conversationTitle conv
                  tag "button"
                    ~ cls "btn btn-icon btn-sm"
                      @ att "type" "button"
                      . att "data-bs-toggle" "modal"
                      . att "data-bs-target" "#deleteConfirmModal"
                    $ tag "i" ~ cls "bi bi-trash"
                    $ none
    tag "hr" none
    tag "div" ~ cls "text-center" $ do
      tag "button"
        ~ cls "btn btn-icon"
          @ att "onClick" "toggleSidebar()"
        $ tag "i" ~ cls "bi bi-arrows-angle-contract fs-5"
        $ none
