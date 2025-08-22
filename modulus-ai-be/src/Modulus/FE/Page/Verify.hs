module Modulus.FE.Page.Verify (page) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Modulus.FE.Utils (homeUrl)
import Modulus.FE.View.VerifyOTPFormView
import Web.Atomic.CSS
import Web.Hyperbole

newtype Preferences = Preferences
  { user_email :: Text
  }
  deriving (Generic, Show, ToQuery, FromQuery)

instance Default Preferences where
  def = Preferences mempty

--- Page
page :: Hyperbole :> es => Eff es (Page '[VerifyOTPFormView])
page = do
  prefs <- query @Preferences
  if T.null (user_email prefs)
    then
      redirect homeUrl
    else do
      pure $ do
        stylesheet "/style.css"
        tag "title" $ text "Verify account"
        el ~ cls "container" $
          el ~ cls "row justify-content-center" $
            el ~ cls "col-md-7 col-lg-5" $
              el ~ cls "registration-container" $ do
                tag "h2" ~ cls "text-center" $ text "Verify Email"
                tag "p" ~ cls "text-center" $
                  text $
                    "A verification code has been sent to "
                      <> user_email prefs
                      <> " . Please enter it below."
                hyper
                  (VerifyOTPFormView 1)
                  ( verifyOTPForm
                      Nothing
                      (user_email prefs)
                      genFields
                  )
