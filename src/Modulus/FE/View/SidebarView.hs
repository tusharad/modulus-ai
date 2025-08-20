module Modulus.FE.View.SidebarView
  ( SidebarView (..)
  , loadSidebarView
  , Action (..)
  ) where

import Control.Monad (forM_, void)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Effectful (IOE)
import Modulus.BE.Client.V1
import Modulus.BE.DB.Internal.Model
import Modulus.BE.Handler.Conversations (deleteConversationHandler)
import Modulus.BE.Log (logDebug)
import Modulus.Common.Utils (runBE, runBEAuth)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Utils
import Web.Atomic.CSS
import Web.Hyperbole

newtype SidebarView = SidebarView Int
  deriving (Generic, ViewId)

instance (IOE :> es, AppConfigEff :> es) => HyperView SidebarView es where
  data Action SidebarView = LoadSidebar | DeleteConv Text
    deriving (Generic, ViewAction)

  update LoadSidebar = do
    eConversationList <- runBEAuth getConversationsHandler
    case eConversationList of
      Left err -> pure $ errorView (T.pack $ show err)
      Right conversationLst -> do
        void . runBE $ logDebug $ "fetched list for sidebar : " <> T.pack (show conversationLst)
        pure $ sidebarView conversationLst
  update (DeleteConv convPublicID_) = do
    case UUID.fromText convPublicID_ of
      Nothing -> pure $ sideBarErrorView "Invalid public ID"
      Just convPublicID -> do
        eRes <-
          runBEAuth
            ( \auth ->
                deleteConversationHandler auth (ConversationPublicID convPublicID)
            )
        case eRes of
          Left err -> pure $ sideBarErrorView (T.pack $ show err)
          Right _ -> update LoadSidebar

errorView :: Text -> View c ()
errorView err = tag "aside" $ do
  el ~ cls "sidebar-header d-flex justify-content-between align-items-center" $ do
    tag "h5" ~ cls "mb-0" $ "History"
  el ~ cls "history-list" $ do
    text err

-- Display message for 1 second then LoadSidebar
sideBarErrorView :: Text -> View SidebarView ()
sideBarErrorView txt =
  el @ onLoad LoadSidebar 1000 $ do
    el ~ cls "text-center py-5" $ do
      text txt

loadSidebarView :: View SidebarView ()
loadSidebarView = do
  el @ onLoad LoadSidebar 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."

sidebarView :: [ConversationRead] -> View SidebarView ()
sidebarView convLst = do
  tag "div" ~ cls "d-flex flex-column flex-shrink-0 p-3 sidebar" @ att "id" "my-sidebar" $ do
    el
      ~ cls
        "d-flex align-items-center mb-3 mb-md-0 me-md-auto \
        \text-white text-decoration-none sidebar-brand"
        @ att "href" "/"
      $ do
        tag "i" ~ cls "bi bi-robot me-2 fs-4" $ none
        tag "span" ~ cls "fs-4 sidebar-brand-text" $ text "Chat History"
    tag "hr" none
    tag "ul" ~ cls "nav nav-pills flex-column mb-auto" $ do
      forM_ convLst $ \conv -> do
        let convPubID = UUID.toText (coerce $ conversationPublicID conv)
        tag "li" ~ cls "nav-item" $ do
          el ~ cls "d-flex align-items-center justify-content-between history-item-text" $ do
            link (chatUrl $ conversationPublicID conv)
              ~ cls "nav-link active d-flex align-items-center flex-grow-1"
              -- TODO: get convID as arg, check if active or not
              $ do
                tag "i" ~ cls "bi bi-chat-dots fs-5 me-2" $ none
                tag "span" ~ cls "sidebar-text" $ text $ conversationTitle conv
            tag "button"
              ~ cls "btn btn-icon btn-sm"
                @ att "type" "button"
                . att "data-bs-toggle" "modal"
                . att "data-bs-target" ("#deleteConfirmModal" <> convPubID)
              $ tag "i" ~ cls "bi bi-trash"
              $ none
    tag "hr" none
    tag "div" ~ cls "d-grid gap-2 text-center" $ do
      tag "button"
        ~ cls "btn btn-primary px-2"
        $ link chatUrlNew ~ cls "text-decoration-none"
        $ text "New chat"
      tag "button"
        ~ cls "btn btn-icon"
          @ att "onClick" "toggleSidebar()"
        $ tag "i" ~ cls "bi bi-arrows-angle-contract fs-5"
        $ none
      deleteConfirmModalView convLst

deleteConfirmModalView :: [ConversationRead] -> View SidebarView ()
deleteConfirmModalView convLst = do
  forM_ convLst $ \conv -> do
    let convPubID = UUID.toText (coerce $ conversationPublicID conv)
     in tag "div"
          ~ cls "modal fade"
            @ att "id" ("deleteConfirmModal" <> convPubID)
            . att "tabindex" "-1"
            . att "aria-labelledby" "deleteConfirmModalLabel"
            . att "aria-hidden" "true"
          $ tag "div" ~ cls "modal-dialog modal-dialog-centered"
          $ do
            tag "div" ~ cls "modal-content" $ do
              tag "div" ~ cls "modal-header" $ do
                tag "h1"
                  ~ cls "modal-title fs-5"
                  $ text "Confirm Deletion"
                tag "button"
                  ~ cls "btn-close"
                    @ att "type" "button"
                    . att "data-bs-dismiss" "modal"
                    . att "aria-label" "Close"
                  $ none
              tag "div" ~ cls "modal-body" $
                text
                  "Are you sure you want to delete this conversation? \
                  \This action cannot be undone."
              tag "div" ~ cls "modal-footer" $ do
                tag "button"
                  ~ cls "btn btn-secondary"
                    @ att "type" "button"
                    . att "data-bs-dismiss" "modal"
                  $ text "Cancel"
                button (DeleteConv convPubID)
                  ~ cls "btn btn-danger"
                    @ att "type" "button"
                    . att "data-bs-dismiss" "modal"
                  $ text "Delete"
