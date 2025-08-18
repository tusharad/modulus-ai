module Modulus.FE.View.RegistrationFormView
  ( RegistrationFormView (..)
  , registrationFormView
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (IOE)
import Modulus.BE.Api.Types
import Modulus.BE.Client.V1
import Modulus.Common.Utils (runBE)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Utils
import qualified Text.Email.Validate as EmailValidate
import Web.Atomic.CSS
import Web.Hyperbole

data RegistrationFormView = RegistrationFormView Int
  deriving (Generic, ViewId)

data RegistrationForm f = RegistrationForm
  { email :: Field f Text
  , password :: Field f Text
  , confirmPassword :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

deriving instance Show (RegistrationForm Identity)
deriving instance ToJSON (RegistrationForm Identity)
deriving instance FromJSON (RegistrationForm Identity)

instance (IOE :> es, AppConfigEff :> es) => HyperView RegistrationFormView es where
  data Action RegistrationFormView
    = SubmitForm
    | RegisterNewUser (RegistrationForm Identity)
    | GoToVerify Text
    deriving (Generic, ViewAction)

  update (GoToVerify userEmail) = redirect $ verifyUrl userEmail
  update (RegisterNewUser RegistrationForm {..}) = do
    let reqBody =
          RegisterRequest
            { registerEmail = email
            , registerPassword = password
            , registerConfirmPassword = confirmPassword
            }
    eRes <- runBE $ registerHandler reqBody
    case eRes of
      Left err -> pure $ registrationFormView (Just . T.pack $ show err) genFields
      Right UserProfile {..} -> pure $ registrationSuccessView userProfileEmail
  update SubmitForm = do
    f <- formData @(RegistrationForm Identity)
    let validatedForm = validateForm f
    if anyInvalid validatedForm
      then
        pure $ registrationFormView Nothing validatedForm
      else pure $ submitRegistrationFormView f

isValidPassword :: String -> Bool
isValidPassword pwd =
  length pwd >= 8
    && any isAsciiUpper pwd
    && any isAsciiLower pwd
    && any isDigit pwd

-- Form validation
anyInvalid :: RegistrationForm Validated -> Bool
anyInvalid RegistrationForm {..} =
  isInvalid email
    || isInvalid password
    || isInvalid confirmPassword

validateForm :: RegistrationForm Identity -> RegistrationForm Validated
validateForm RegistrationForm {..} =
  RegistrationForm
    { email =
        validate
          (isLeft $ EmailValidate.validate $ TE.encodeUtf8 email)
          "Email is invalid"
    , password =
        validate
          (not $ isValidPassword (T.unpack password))
          "Password not valid"
    , confirmPassword =
        validate
          (password /= confirmPassword)
          "Password and confirm Password are not matching"
    }

registrationSuccessView :: Text -> View RegistrationFormView ()
registrationSuccessView userEmail =
  el ~ cls "mb-3" @ onLoad (GoToVerify userEmail) 200 $
    tag "label" ~ cls "form-label" $
      text "Registration successful"

registrationFormView ::
  Maybe Text ->
  RegistrationForm Validated ->
  View RegistrationFormView ()
registrationFormView mbErrorMsg r = do
  let f = fieldNames @RegistrationForm
  case mbErrorMsg of
    Nothing -> none
    Just errMsg ->
      el ~ cls "mb-3" $ do
        el ~ cls "invalid-feedback d-block" $ text errMsg
  myForm SubmitForm $ do
    el ~ cls "mb-3" $ do
      tag "label" ~ cls "form-label" $ text "Email"
      field (email f) $ input Email ~ cls "form-control"
      checkInvalid (email r)

    el ~ cls "mb-3" $ do
      tag "label" ~ cls "form-label" $ text "Password"
      field (password f) $ input CurrentPassword ~ cls "form-control"
      checkInvalid (password r)

    el ~ cls "mb-3" $ do
      tag "label" ~ cls "form-label" $ text "Confirm Password"
      field (confirmPassword f) $ input CurrentPassword ~ cls "form-control"
      checkInvalid (confirmPassword r)

    el ~ cls "d-grid" $ do
      submit ~ cls "btn btn-register" $ text "Sign up"
  where
    checkInvalid x =
      case x of
        Invalid errMsg -> el ~ cls "invalid-feedback d-block" $ text errMsg
        _ -> none

{-
Handle post validated form submission
-}
submitRegistrationFormView :: RegistrationForm Identity -> View RegistrationFormView ()
submitRegistrationFormView r = do
  el @ onLoad (RegisterNewUser r) 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."
      el ~ cls "mt-3 mb-0" $ text "Creating your account..."
