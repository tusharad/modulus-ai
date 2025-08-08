{-# LANGUAGE QuasiQuotes #-}

module Modulus.FE.Utils
  ( myForm
  , homeUrl
  , notFoundUrl
  ) where

import Network.URI.Static
import Web.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.HyperView.Forms

homeUrl :: URI
homeUrl = [relativeReference|/|]

notFoundUrl :: URI
notFoundUrl = [relativeReference|/not-found|]

-- form but without col css
myForm :: (ViewAction (Action id)) => Action id -> View (FormFields id) () -> View id ()
myForm a cnt = do
  vid <- context
  tag "form" @ onSubmit a $ do
    addContext (FormFields vid) cnt
