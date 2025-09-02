module Modulus.BE.Service.Conversation (updateConversationTitle) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Modulus.BE.Api.Types
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.ChatMessage (getChatMessagesByConvID)
import Modulus.BE.DB.Queries.Conversation (getConversationsByPublicID)
import Modulus.BE.LLM
  ( AnyLLMProvider (AnyLLMProvider)
  , LLMProvider (generateNewConversationTitle)
  , mkLLMProvider
  )
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
              _ <-
                liftIO $
                  generateNewConversationTitle
                    llmProvider
                    (chatMessageContent firstMsg)
              pure ()
