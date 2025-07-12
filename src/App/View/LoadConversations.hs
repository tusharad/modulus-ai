module App.View.LoadConversations (
    LoadConversationsView (..)
  , Action (..)
  , renderLoadingConversations
) where

import App.View.Chat
import App.View.GenerateReply
import App.DB
import Effectful
import Web.Hyperbole

data LoadConversationsView = LoadConversationsView Int
  deriving (Generic, ViewId)

instance (IOE :> es) => HyperView LoadConversationsView es where
  data Action LoadConversationsView = LoadHistory
    deriving (Generic, ViewAction)

  update LoadHistory = do
    (LoadConversationsView chatId) <- viewId
    conversations <- liftIO $ loadConversations
    pure $ renderSidebarHistory chatId conversations

loadConversations :: IO [Conversation]
loadConversations = withDatabase "chat.db" $ \conn -> do
  conversationsWithCount <- getConversationsWithMessageCount conn
  pure $ map cwcConversation conversationsWithCount

renderLoadingConversations :: View LoadConversationsView ()
renderLoadingConversations = el @ onLoad LoadHistory 300 $ text "Loading conversations..."
