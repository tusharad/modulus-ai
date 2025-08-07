module Modulus.FE.Page.Chat (page) where

import Effectful
import Web.Hyperbole

--- Page
page :: Int -> Eff es (Page '[])
page _ = do
  pure $ do
    stylesheet "/style.css"
    el $ text "hello world"
