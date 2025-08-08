module Modulus.FE.View.LoginFormView
  ( 
    LoginFormView (..)
  , loginFormView
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (IOE)
import Modulus.BE.Api.Types
import Modulus.BE.Client.V1
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Utils
import qualified Text.Email.Validate as EmailValidate
import Web.Atomic.CSS
import Web.Hyperbole
import Modulus.Common.Utils (runBE)

data LoginFormView = LoginFormView Int
  deriving (Generic, ViewId)

data LoginForm f = LoginForm
  { email :: Field f Text
  , password :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

deriving instance Show (LoginForm Identity)
deriving instance ToJSON (LoginForm Identity)
deriving instance FromJSON (LoginForm Identity)

instance (IOE :> es, AppConfigEff :> es) => HyperView LoginFormView es where
  data Action LoginFormView
    = SubmitForm
    | LoginUser (LoginForm Identity)
    | GoToHome
    deriving (Generic, ViewAction)

  update GoToHome = redirect homeUrl

  update (LoginUser LoginForm{..}) = do
    let reqBody =
          LoginRequest
            { loginEmail = email
            , loginPassword = password
            }
    eRes <- runBE $ loginHandler reqBody
    case eRes of
      Left err -> pure $ loginFormView (Just . T.pack $ show err) genFields
      Right authTokens -> do 
        saveSession authTokens
        pure loginSuccessView

  update SubmitForm = do
    f <- formData @(LoginForm Identity)
    let validatedForm = validateForm f
    if anyInvalid validatedForm
      then
        pure $ loginFormView Nothing validatedForm
      else pure $ submitLoginFormView f

loginSuccessView :: View LoginFormView ()
loginSuccessView = 
  el ~ cls "mb-3" @ onLoad GoToHome 200 $ 
      tag "label" ~ cls "form-label" $ text "Login successful"


isValidPassword :: String -> Bool
isValidPassword pwd =
  length pwd >= 8
    && any isAsciiUpper pwd
    && any isAsciiLower pwd
    && any isDigit pwd

-- Form validation
anyInvalid :: LoginForm Validated -> Bool
anyInvalid LoginForm {..} = isInvalid email || isInvalid password

validateForm :: LoginForm Identity -> LoginForm Validated
validateForm LoginForm {..} =
  LoginForm
    { email =
        validate
          (isLeft $ EmailValidate.validate $ TE.encodeUtf8 email)
          "Email is invalid"
    , password = validate 
        (not $ isValidPassword (T.unpack password)) "Password not valid"
    }

{-
Handle post validated form submission
-}
submitLoginFormView :: LoginForm Identity -> View LoginFormView ()
submitLoginFormView r = do
  el @ onLoad (LoginUser r) 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."
      el ~ cls "mt-3 mb-0" $ text "Logging in..."

loginFormView ::
  Maybe Text ->
  LoginForm Validated ->
  View LoginFormView ()
loginFormView mbErrorMsg r = do
  let f = fieldNames @LoginForm
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
      field (password f) $ input NewPassword ~ cls "form-control"
      checkInvalid (password r)

    el ~ cls "d-grid" $ do
      submit ~ cls "btn btn-register" $ text "Login"
  where
    checkInvalid x =
      case x of
        Invalid errMsg -> el ~ cls "invalid-feedback d-block" $ text errMsg
        _ -> none


