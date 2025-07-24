module App.Page.Chat (page) where

import App.Common.Utils
import App.Effects.StateStore
import App.View.Chat
import App.View.GenerateReply
import App.View.PromptInput
import Effectful
import Web.Atomic.CSS
import Web.Hyperbole
import App.View.LoadConversations 
import App.View.ChatMessageView
import App.View.ModelProviders

--- Page
page ::
  (StateStoreEff :> es, IOE :> es) =>
  Int ->
  Eff
    es
    ( Page
        '[ LoadConversationsView
         , ChatMessagesView
         , PromptInputView
         , GenerateReplyView
         , ModelProviders
         ]
    )
page chatId = do
  providerInfo <- getProviderInfo
  ollamaModels <- getAvailableOllamaModels
  orModels <- getAvailableORModels
  pure $ do
    stylesheet "/style.css"
    el ~ cls "main-container" $ do
      el ~ cls "sidebar" @ att "id" "sidebar" $ do
        renderSidebarHeader
        el ~ cls "sidebar-area" $ 
          hyper (LoadConversationsView chatId) $ renderLoadingConversations
      el ~ cls "main-content" $ do
        el ~ cls "top-bar" $ do
          renderTopBarLeft
          myHyper (ModelProviders 1) $ 
            renderProviderListView providerInfo ollamaModels orModels
        hyper (ChatMessagesView chatId) $ renderLoadingMessages
        myHyper (PromptInputView chatId) $ renderPromptInputArea genFields
    script "/chat_page.js"
