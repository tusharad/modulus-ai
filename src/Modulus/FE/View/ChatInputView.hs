module Modulus.FE.View.ChatInputView
  ( chatInputView
  , ChatInputView (..)
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (IOE)
import Modulus.BE.Log (logDebug)
import Modulus.Common.Utils (runBE)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Effects.StateStore
import Modulus.FE.Utils
import Modulus.FE.View.ChatView
import Web.Atomic.CSS
import Web.Hyperbole

newtype ChatInputView = ChatInputView Text
  deriving (Generic, ViewId)

newtype ChatForm f = ChatForm
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
    el @ onLoad ProcessPrompt 200 $
      none
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
  myForm SubmitInput ~ cls "input-group" $ do
    checkInvalid (chatPrompt r)
    field (chatPrompt f) $ do
      textarea Nothing
        ~ cls "form-control form-control-dark"
          @ att "rows" "1"
    tag "button"
      ~ cls "btn btn-icon"
        @ att "type" "button"
        . att "data-bs-toggle" "dropdown"
        . att "aria-expanded" "false"
        . att "title" "Tools"
      $ tag "i" ~ cls "bi bi-tools fs-5"
      $ none
    tag "ul"
      ~ cls "dropdown-menu dropdown-menu-end"
        @ att "id" "tools-dropdown"
      $ do
        tag "li" $
          tag "a" ~ cls "dropdown-item" $
            tag "i" ~ cls "bi bi-search me-2" $
              text "Web Search"
        tag "li" $
          tag "a" ~ cls "dropdown-item" $
            tag "i" ~ cls "bi bi-wikipedia me-2" $
              text "Wikipedia"
        tag "li" $
          tag "a" ~ cls "dropdown-item" $
            tag "i" ~ cls "bi bi-paperclip me-2" $
              text "Upload File"
    submit ~ cls "btn btn-primary" $
      tag "i" ~ cls "bi bi-send-fill" $
        none
  where
    checkInvalid x =
      case x of
        Invalid errMsg -> el ~ cls "invalid-feedback d-block" $ text errMsg
        _ -> none

chatInputView :: View ChatInputView ()
chatInputView = do
  tag "footer" ~ cls "p-3 border-top" $ do
    chatFormView Nothing genFields
