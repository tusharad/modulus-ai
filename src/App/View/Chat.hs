{-# LANGUAGE OverloadedStrings #-}

{- A module to hold stateless views for Chat -}
module App.View.Chat
  ( renderConversationPreview
  , renderSidebarHistory
  , renderMessageList
  , renderMessageBubble
  , renderEmptyChatView
  , renderSidebarHeader
  , renderTopBarLeft
  ) where

import App.Common.Utils
import App.DB
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Web.Atomic.CSS
import Web.Hyperbole
import App.Common.MarkdownAtomic (parseView)

-- | Left side of the top bar
renderTopBarLeft :: View c ()
renderTopBarLeft = do
  el ~ cls "top-bar-left" $ do
    tag "button" ~ cls "sidebar-toggle" @ att "onClick" "toggleSidebar()" $ do
      tag "i" ~ cls "fas fa-bars" $ none
    tag "h6" ~ cls "mb-0" $ "AI Assistant"

-- | A single chat item in the history
renderConversationPreview :: Text -> Bool -> View c ()
renderConversationPreview title isActive = do
  el ~ cls ("chat-item" <> if isActive then " active" else "") $ do
    el ~ cls "chat-item-title" $ text title

-- | Chat history section
renderSidebarHistory :: Int -> [Conversation] -> View c ()
renderSidebarHistory chatId lst = do
  forM_ lst $ \conv -> do
    link (relUrl $ "/chat/" <> (T.pack $ show $ convId conv)) $
      renderConversationPreview (convTitle conv) (convId conv == chatId)

renderMessageList :: [ChatMessage] -> View c ()
renderMessageList [] = renderEmptyChatView
renderMessageList lst = do
  el ~ cls "chat-area" @ att "id" "chatArea" $ do
    forM_ lst $ \chatMsg -> do
      renderMessageBubble (parseView $ msgContent chatMsg) (msgRole chatMsg == User)

-- | Sidebar header with title and new chat button
renderSidebarHeader :: View c ()
renderSidebarHeader = do
  el ~ cls "sidebar-header" $ do
    tag "h5" $ do
      tag "i" ~ cls "fas fa-comments" $ none
      text " Chat History"
    link (relUrl "/chat")
      ~ cls "btn new-chat-btn"
        @ att "onClick" "startNewChat()"
      $ do
        tag "i" ~ cls "fas fa-plus" $ none
        text " New Chat"

-- \| A single example prompt card
renderExamplePromptCard :: Text -> Text -> Text -> View c ()
renderExamplePromptCard _ title description = do
  el
    ~ cls "example-prompt"
      @ att "onClick" "setPrompt"
    $ do
      tag "h6" $ text title
      tag "p" $ text description

-- \| The full empty state view (hidden by default)
renderEmptyChatView :: View c ()
renderEmptyChatView = do
  el
    ~ cls "empty-state"
      @ att "id" "emptyState"
      . att "style" "display None"
    $ do
      -- Icon
      el ~ cls "empty-state-icon" $ do
        tag "i" ~ cls "fas fa-robot" $ none

      -- Title and Description
      tag "h2" $ text "Welcome to AI Assistant"
      tag "p" $ text "Start a conversation with your intelligent AI companion"

      -- Example Prompts Section
      el ~ cls "example-prompts" $ do
        renderExamplePromptCard
          "Explain quantum computing in simple terms"
          "Explain Complex Topics"
          $ "Break down difficult concepts into easy-to-understand explanations"

        renderExamplePromptCard
          "Write a Python function to sort a list"
          "Code Assistance"
          $ "Get help with programming tasks and code optimization"

        renderExamplePromptCard "Plan a trip to Tokyo for 5 days" "Trip Planning" $
          "Create detailed itineraries and travel recommendations"

        renderExamplePromptCard "Write a professional email" "Writing Help" $
          "Assist with various writing tasks and content creation"

-- \| A chat message view, with user or AI styling
renderMessageBubble :: View c () -> Bool -> View c ()
renderMessageBubble content isUser = do
  el ~ cls "chat-message" $ do
    el ~ cls ("message-wrapper" <> if isUser then " user" else " assistant") $ do
      el ~ cls ("message-avatar" <> if isUser then " user" else " assistant") $ do
        text (if isUser then "U" else "A")
      el ~ cls ("message-content" <> if isUser then " user" else " assistant") $ do
        el ~ cls "message-text" $ content
