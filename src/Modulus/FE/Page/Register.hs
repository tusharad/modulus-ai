module Modulus.FE.Page.Register (page) where

import Effectful
import Web.Hyperbole
import Web.Atomic.CSS
import Modulus.FE.View.RegistrationFormView 

--- Page
page :: Eff es (Page '[RegistrationFormView])
page = do
  pure $ do
    stylesheet "/style.css"
    tag "title" $ text "Create your account"
    el ~ cls "container" $ 
      el ~ cls "row justify-content-center" $
        el ~ cls "col-md-7 col-lg-5" $ 
          el ~ cls "registration-container" $ do
              tag "h2" ~ cls "text-center" $ text "Create Account"
              hyper (RegistrationFormView 1) $ registrationFormView Nothing genFields
    script "/register.js"
