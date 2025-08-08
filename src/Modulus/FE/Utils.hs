{-# LANGUAGE QuasiQuotes #-}

module Modulus.FE.Utils
  ( myForm
  , homeUrl
  , notFoundUrl
  , loginUrl
  , verifyUrl 
  ) where

import Data.Text (Text)
import Network.URI.Static
import Web.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.HyperView.Forms
import qualified Data.Text as T
import Network.URI (parseRelativeReference, nullURI)
import Data.Maybe (fromMaybe)

homeUrl :: URI
homeUrl = [relativeReference|/|]

loginUrl :: URI
loginUrl = [relativeReference|/login|]

notFoundUrl :: URI
notFoundUrl = [relativeReference|/not-found|]

verifyUrl :: Text -> URI
verifyUrl user_email =
  let str = "/verify?user_email=" <> user_email
   in fromMaybe nullURI $ parseRelativeReference (T.unpack str)

-- form but without col css
myForm :: (ViewAction (Action id)) => Action id -> View (FormFields id) () -> View id ()
myForm a cnt = do
  vid <- context
  tag "form" @ onSubmit a $ do
    addContext (FormFields vid) cnt
