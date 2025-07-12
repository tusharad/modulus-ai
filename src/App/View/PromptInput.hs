module App.View.PromptInput
  ( PromptInputView (..)
  , Action (..)
  , renderPromptInputArea
  ) where

import App.Common.Types
import App.Common.Utils
import App.View.ChatMessageView
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Web.Atomic.CSS
import Web.Hyperbole

data PromptInputView = PromptInputView Int
  deriving (Generic, ViewId)

data PromptForm f = PromptForm
  { prompt :: Field f Text
  , hasFile :: Field f (Maybe Text)
  , selectedTool :: Field f (Maybe AvailableTool)
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

deriving instance Show (PromptForm Identity)

instance (IOE :> es) => HyperView PromptInputView es where
  data Action PromptInputView = SubmitPrompt
    deriving (Generic, ViewAction)

  type Require PromptInputView = '[ChatMessagesView]

  update SubmitPrompt = do
    (PromptInputView chatId) <- viewId
    f@PromptForm{..} <- formData @(PromptForm Identity)
    liftIO $ putStrLn $ "form submitted: " <> show f
    let validatedForm = validateForm f
    if anyInvalid validatedForm
      then pure $ renderPromptInputArea validatedForm
      else pure $ submitPromptAndRenderInput chatId prompt hasFile selectedTool

-- Form validation
anyInvalid :: PromptForm Validated -> Bool
anyInvalid PromptForm {..} = or [isInvalid prompt, isInvalid hasFile, isInvalid selectedTool]

validateForm :: PromptForm Identity -> PromptForm Validated
validateForm PromptForm {..} =
  PromptForm
    { prompt = validate (T.length prompt < 1) "Prompt should not be empty"
    , hasFile = NotInvalid
    , selectedTool = NotInvalid
    }

renderPromptInputArea :: PromptForm Validated -> View PromptInputView ()
renderPromptInputArea p = do
  let f = fieldNames @PromptForm
  el ~ cls "input-area" $ do
    myForm SubmitPrompt $ do
      el ~ cls "input-container" $ do
        case prompt p of
          Invalid t -> el (text t)
          _ -> none
        el ~ cls "input-wrapper" $ do
          field (prompt f) $ do
            textarea Nothing ~ cls "message-input" @ placeholder "Type your message..."
                . att "rows" "3"
          el ~ cls "input-actions" $ do
            submit ~ cls "send-btn" @ att "id" "send-btn" $
              tag "i" ~ cls "fas fa-paper-plane" $ none
        el ~ cls "input-options" $ do
          tag "input" @ att "type" "file" @ att "id" "document-upload" $ none
          field (hasFile f) $ input TextInput @ att "hidden" "" . value ""
          field (selectedTool f) $ do
            select ~ cls "form-select model-select" $ do
              tag "option" @ value "" $ "Select Tool"
              tag "option" @ value "Web" $ "Web search"
              tag "option" @ value "Wiki" $ "Wikipedia"

submitPromptAndRenderInput ::
  Int -> Text -> Maybe Text -> Maybe AvailableTool -> View PromptInputView ()
submitPromptAndRenderInput chatId p mbFilePath mbTool = do
  target (ChatMessagesView chatId) $ el @ onLoad (AddChat p mbFilePath mbTool) 300 $ none
  renderPromptInputArea genFields
