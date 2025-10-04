module Modulus.BE.Service.Conversation (updateConversationTitle) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Langchain.Utils (showText)
import Modulus.BE.Api.Types (LLMRespStreamBody)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.ChatMessage (getChatMessagesByConvID)
import Modulus.BE.DB.Queries.Conversation (getConversationsByPublicID, updateConversation)
import Modulus.BE.LLM
  ( AnyLLMProvider (AnyLLMProvider)
  , LLMProvider (generateNewConversationTitle)
  , NewConversationTitle (..)
  , mkLLMProvider
  )
import Modulus.BE.Log (logDebug)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Error (AppError (ExternalServiceError, NotFoundError))
import Servant (throwError)

updateConversationTitle :: ConversationPublicID -> LLMRespStreamBody -> AppM ()
updateConversationTitle convPublicID llmRespBody = do
  mbConversationRead <- getConversationsByPublicID convPublicID
  case mbConversationRead of
    Nothing -> throwError $ NotFoundError "Conversation not found"
    Just convRead -> do
      chatMsgs <- getChatMessagesByConvID (conversationID convRead)
      case chatMsgs of
        [] -> throwError $ NotFoundError "Chat messages are empty or invalid convID"
        (firstMsg : _) -> do
          eLLM <- mkLLMProvider llmRespBody
          case eLLM of
            Left e -> throwError $ ExternalServiceError e
            Right (AnyLLMProvider llmProvider) -> do
              eRes <-
                liftIO $
                  generateNewConversationTitle
                    llmProvider
                    (chatMessageContent firstMsg)
              case eRes of
                Left err -> logDebug $ "Failed to generate new title: " <> showText err
                Right (NewConversationTitle newTitle) -> do
                  let updatedConv =
                        convRead
                          { conversationTitle = newTitle
                          , conversationID = ()
                          , conversationPublicID = ()
                          , conversationCreatedAt = ()
                          , conversationUpdatedAt = ()
                          }
                  updateConversation (conversationID convRead) updatedConv
                  logDebug $ "Title updated for conversation ID " <> T.pack (show convPublicID)
