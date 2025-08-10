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
import Modulus.Common.Utils (runBEAuth, runBE)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Utils
import Web.Atomic.CSS
import Web.Hyperbole
import Modulus.BE.Log (logDebug)

data SidebarView = SidebarView Int
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

chatDots :: View c ()
chatDots = tag "i" ~ cls "bi bi-chat-dots me-2" $ none

chatDotsFilled :: View c ()
chatDotsFilled = tag "i" ~ cls "bi bi-chat-dots-fill me-2" $ none

sidebarView :: [ConversationRead] -> View SidebarView ()
sidebarView convLst = do
  tag "aside" ~ cls "my-sidebar" $ do
    el ~ cls "sidebar-header d-flex justify-content-between align-items-center" $ do
      tag "h5" ~ cls "mb-0" $ "History"
    el ~ cls "history-list" $ do
      forM_ convLst $ \conv -> do
        link (chatUrl $ conversationPublicID conv) ~ cls "history-item" $ do
          chatDots
          text $ conversationTitle conv
    el ~ cls "sidebar-footer p-3 border-top border-secondary-subtle" $ do
      link chatUrlNew ~ cls "btn btn-outline-light w-100" $ do
        tag "i" ~ cls "bi bi-plus-lg me-2" $ none
        text "New Chat"
