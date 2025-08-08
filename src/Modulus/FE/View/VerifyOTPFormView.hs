module Modulus.FE.View.VerifyOTPFormView
  ( VerifyOTPFormView (..)
  , verifyOTPForm
  ) where

import Control.Monad (void)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (IOE, MonadIO (liftIO))
import Modulus.BE.Api.Types (OTPVerifyRequest (..))
import Modulus.BE.Handler.Auth (verifyOTPHandler)
import Modulus.BE.Log (logDebug)
import Modulus.BE.Monad.AppM (runAppM)
import Modulus.FE.Effects.AppConfig (AppConfigEff, getAppCfg)
import Modulus.FE.Utils
import qualified Text.Email.Validate as EmailValidate
import Web.Atomic.CSS
import Web.Hyperbole

data VerifyOTPFormView = VerifyOTPFormView Int
  deriving (Generic, ViewId)

data VerifyOTPForm f = VerifyOTPForm
  { otp :: Field f Int
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)

deriving instance Show (VerifyOTPForm Identity)
deriving instance Show (VerifyOTPForm Validated)
deriving instance FromJSON (VerifyOTPForm Identity)
deriving instance ToJSON (VerifyOTPForm Identity)

instance (IOE :> es, AppConfigEff :> es) => HyperView VerifyOTPFormView es where
  data Action VerifyOTPFormView
    = SubmitForm Text
    | VerifyingOTP (VerifyOTPForm Identity) Text
    | GoToLogin
    deriving (Generic, ViewAction)

  update GoToLogin = redirect loginUrl
  update (SubmitForm userEmail) = do
    f <- formData @(VerifyOTPForm Identity)
    let validatedForm = validateForm f
    cfg <- getAppCfg
    void . liftIO $ runAppM cfg $ logDebug $ "Reached here " <> T.pack (show f)
    if anyInvalid validatedForm
      || isLeft (EmailValidate.validate $ TE.encodeUtf8 userEmail)
      then do
        void . liftIO $
          runAppM cfg $
            logDebug $
              "error happend "
                <> T.pack (show validatedForm)
        pure $ verifyOTPForm Nothing userEmail validatedForm
      else pure $ submitVerifyView f userEmail
  update (VerifyingOTP VerifyOTPForm {..} userEmail) = do
    let reqBody =
          OTPVerifyRequest
            { verifyEmail = userEmail
            , verifyOTP = otp
            }
    cfg <- getAppCfg
    eRes <- liftIO $ runAppM cfg $ verifyOTPHandler reqBody
    case eRes of
      Left err ->
        pure $
          verifyOTPForm
            (Just . T.pack $ show err)
            userEmail
            genFields
      Right _ -> pure verificationSuccessView

verificationSuccessView :: View VerifyOTPFormView ()
verificationSuccessView =
  el ~ cls "mb-3" @ onLoad GoToLogin 200 $
    tag "label" ~ cls "form-label" $
      text "Account has been verified. Redirecting to login"

-- Form validation
anyInvalid :: VerifyOTPForm Validated -> Bool
anyInvalid VerifyOTPForm {..} = isInvalid otp

validateForm :: VerifyOTPForm Identity -> VerifyOTPForm Validated
validateForm VerifyOTPForm {..} =
  VerifyOTPForm
    { otp =
        validate
          (not $ otp >= 100000 && otp <= 999999)
          "Password not valid"
    }

submitVerifyView :: VerifyOTPForm Identity -> Text -> View VerifyOTPFormView ()
submitVerifyView v userEmail = do
  el @ onLoad (VerifyingOTP v userEmail) 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."
      el ~ cls "mt-3 mb-0" $ text "Verifying your account..."

verifyOTPForm ::
  Maybe Text ->
  Text ->
  VerifyOTPForm Validated ->
  View VerifyOTPFormView ()
verifyOTPForm mbErrorMsg uEmail r = do
  let f = fieldNames @VerifyOTPForm
  case mbErrorMsg of
    Nothing -> none
    Just errMsg ->
      el ~ cls "mb-3" $ do
        el ~ cls "invalid-feedback d-block" $ text errMsg

  myForm (SubmitForm uEmail) $ do
    el ~ cls "mb-3" $ do
      tag "label" ~ cls "form-label" $ text "Email"
      field "user_email" $
        input Email
          ~ cls "form-control"
            @ att "disabled" ""
            . att "value" uEmail

    el ~ cls "mb-3" $ do
      tag "label" ~ cls "form-label" $ text "OTP"
      field (otp f) $ input Number ~ cls "form-control"
      checkInvalid (otp r)

    el ~ cls "d-grid" $ do
      submit ~ cls "btn btn-register" $ text "Verify Account"
  where
    checkInvalid x =
      case x of
        Invalid errMsg -> el ~ cls "invalid-feedback d-block" $ text errMsg
        _ -> none
