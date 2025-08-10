module Modulus.FE.View.ChatInputView
  ( chatInputView
  , ChatInputView (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful (IOE)
import Modulus.BE.Log (logDebug)
import Modulus.Common.Utils (runBE)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Effects.StateStore (StateStore (..), StateStoreEff, modifyState)
import Modulus.FE.Utils
import Modulus.FE.View.ChatView
import Web.Atomic.CSS
import Web.Hyperbole
import Control.Monad (void)

data ChatInputView = ChatInputView Text
  deriving (Generic, ViewId)

data ChatForm f = ChatForm
  { chatPrompt :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

deriving instance Show (ChatForm Identity)
deriving instance ToJSON (ChatForm Identity)
deriving instance FromJSON (ChatForm Identity)

instance
  ( IOE :> es
  , StateStoreEff :> es
  , AppConfigEff :> es
  ) =>
  HyperView ChatInputView es
  where
  data Action ChatInputView = SubmitInput
    deriving (Generic, ViewAction)

  type Require ChatInputView = '[ChatView]

  update SubmitInput = do
    (ChatInputView convID) <- viewId
    f <- formData @(ChatForm Identity)
    void . runBE $ logDebug $ "submitted input for " <> convID
    let validatedForm = validateForm f
    if anyInvalid validatedForm
      then pure $ chatFormView Nothing validatedForm
      else do
        modifyState $ \st -> st {currentPrompt = chatPrompt f}
        pure $ processPrompt convID

-- Form validation
anyInvalid :: ChatForm Validated -> Bool
anyInvalid ChatForm {..} = isInvalid chatPrompt

validateForm :: ChatForm Identity -> ChatForm Validated
validateForm ChatForm {..} =
  ChatForm
    { chatPrompt =
        validate
          (T.null chatPrompt)
          "Input cannot be empty"
    }

processPrompt ::
  Text -> View ChatInputView ()
processPrompt convID = do
  target (ChatView convID) $
    el @ onLoad ProcessPrompt 200 $ none
  chatFormView Nothing genFields

chatFormView ::
  Maybe Text ->
  ChatForm Validated ->
  View ChatInputView ()
chatFormView mbErrorMsg r = do
  let f = fieldNames @ChatForm
  case mbErrorMsg of
    Nothing -> none
    Just errMsg ->
      el ~ cls "mb-3" $ do
        el ~ cls "invalid-feedback d-block" $ text errMsg
  myForm SubmitInput $ do
    el ~ cls "input-wrapper" $ do
      checkInvalid (chatPrompt r)
      field (chatPrompt f) $
        textarea Nothing
          ~ cls "form-control userInput"
            @ att "rows" "1"
      submit ~ cls "btn btn-primary rounded-circle p-2 sendButton" $
        tag "i" ~ cls "bi bi-arrow-up" $
          none
  where
    checkInvalid x =
      case x of
        Invalid errMsg -> el ~ cls "invalid-feedback d-block" $ text errMsg
        _ -> none

chatInputView :: View ChatInputView ()
chatInputView = do
  chatFormView Nothing genFields
  el ~ cls "tool-options" $ do
    tag "button" ~ cls "btn tool-btn" $ tag "i" ~ cls "bi bi-paperclip" $ none
    tag "button" ~ cls "btn tool-btn" $ do
      tag "i" ~ cls "bi bi-search me-1" $ none
      text "Web search"
    tag "button" ~ cls "btn tool-btn active" $ do
      tag "i" ~ cls "bi bi-lightbulb me-1" $ none
      text "Thinking"
    tag "button" ~ cls "btn tool-btn" $ do
      tag "i" ~ cls "bi bi-wikipedia me-1" $ none
      text "Wikipedia"
