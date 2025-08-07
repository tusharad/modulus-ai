module Modulus.FE.Page.Register (page) where

import Effectful
import Web.Hyperbole
import Modulus.FE.Layout (mainBackground)

--- Page
page :: Eff es (Page '[])
page = do
  pure $ do
    stylesheet "/style.css"
    mainBackground $ do
        text "hi"
