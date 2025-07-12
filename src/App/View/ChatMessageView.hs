module App.View.ChatMessageView (
    ChatMessagesView (..)
   , Action(..)
   , renderLoadingMessages
) where

import App.Common.Types
import App.View.Chat
import App.View.GenerateReply
import App.DB
import App.LLM
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forM_, void)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Web.Atomic.CSS
import Web.Hyperbole
import App.View.LoadConversations
import App.Common.MarkdownAtomic (parseView)

data ChatMessagesView = ChatMessagesView Int
  deriving (Generic, ViewId)

instance (IOE :> es) => HyperView ChatMessagesView es where
  data Action ChatMessagesView = LoadChat 
        | AddChat Text (Maybe Text) (Maybe AvailableTool)
    deriving (Generic, ViewAction)

  type Require ChatMessagesView = '[GenerateReplyView, LoadConversationsView]

  update = \case
    LoadChat -> do 
        (ChatMessagesView chatId) <- viewId
        loadChatMessages chatId
    AddChat p mbFilePath mbTool -> do 
        (ChatMessagesView chatId) <- viewId
        addChatMessage chatId p mbFilePath mbTool

loadChatMessages :: (IOE :> es) => Int -> Eff es (View ChatMessagesView ())
loadChatMessages chatId = do
  messages <- liftIO $ withDatabase "chat.db" $ \conn ->
    getConversationMessages conn chatId
  pure $ renderMessageList messages

addChatMessage ::
  (IOE :> es) =>
  Int ->
  Text ->
  Maybe Text ->
  Maybe AvailableTool ->
  Eff es (View ChatMessagesView ())
addChatMessage chatId_ p mbFilePath mbTool = do
  eChatId <- resolveOrCreateChat chatId_ p
  case eChatId of
    Left err -> pure $ renderMessageBubble (text $ T.pack err) False
    Right chatId -> do
      messages <- liftIO $ addMessageToChat chatId p mbFilePath
      pure $ renderMessagesWithPendingReply p mbFilePath mbTool chatId messages

resolveOrCreateChat :: (IOE :> es) => Int -> Text -> Eff es (Either String Int)
resolveOrCreateChat chatId_ p
  | chatId_ /= 0 = pure $ Right chatId_
  | otherwise = liftIO $ do
      eRes <- try $ withDatabase "chat.db" $ \conn ->
        createConversation conn "new conversation begins..."
      case eRes of
        Left err -> pure $ Left $ show (err :: SomeException)
        Right chatId -> do
          void $ forkIO $ generateAndInsertTitle p chatId
          pure $ Right chatId

addMessageToChat :: Int -> Text -> Maybe Text -> IO [ChatMessage]
addMessageToChat chatId p mbFilePath = withDatabase "chat.db" $ \conn -> do
  void $ addMessage conn User p chatId mbFilePath
  getConversationMessages conn chatId

generateAndInsertTitle :: Text -> Int -> IO ()
generateAndInsertTitle userQuery chatId = do
  eRes <- generateTitle userQuery
  case eRes of
    Left err -> putStrLn err
    Right newTitle -> do
      void $ withDatabase "chat.db" $ \conn -> do
        updateConversationTitle conn chatId newTitle
          `catch` (\e -> print (e :: SomeException))

renderMessagesWithPendingReply ::
  Text ->
  Maybe Text ->
  Maybe AvailableTool ->
  Int ->
  [ChatMessage] ->
  View ChatMessagesView ()
renderMessagesWithPendingReply _ _ _ _ [] = renderEmptyChatView
renderMessagesWithPendingReply p mbFilePath mbTool chatId messages = do
  target (LoadConversationsView 0) $ el @ onLoad LoadHistory 300 $ none
  el ~ cls "chat-area" @ att "id" "chatArea" $ do
    forM_ messages $ \chatMsg ->
      renderMessageBubble (parseView $ msgContent chatMsg) (msgRole chatMsg == User)
    hyper (GenerateReplyView chatId) $ renderMessageBubble "Generating reply..." False
    target (GenerateReplyView chatId) $
      el @ onLoad (Generate p mbFilePath mbTool) 300 $ none

renderLoadingMessages :: View ChatMessagesView ()
renderLoadingMessages = el @ onLoad LoadChat 300 $ text "Loading messages..."
