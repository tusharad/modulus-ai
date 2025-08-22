module Modulus.FE.Page.Login (page) where

import Effectful
import Modulus.FE.View.LoginFormView
import Web.Atomic.CSS
import Web.Hyperbole

--- Page
page :: Eff es (Page '[LoginFormView])
page = do
  pure $ do
    stylesheet "/style.css"
    tag "title" $ text "Login"
    el ~ cls "container" $
      el ~ cls "row justify-content-center" $
        el ~ cls "col-md-7 col-lg-5" $
          el ~ cls "registration-container" $ do
            tag "h2" ~ cls "text-center" $ text "Create Account"
            hyper (LoginFormView 1) $ loginFormView Nothing genFields
