{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Modulus.FE.Utils
  ( myForm
  , homeUrl
  , notFoundUrl
  , loginUrl
  , verifyUrl 
  , chatUrl 
  , chatUrlNew 
  , myHyper
  ) where

import Data.Text (Text)
import Network.URI.Static
import Web.Hyperbole
import Web.Hyperbole.HyperView
import Web.Hyperbole.HyperView.Forms
import qualified Data.Text as T
import Network.URI (parseRelativeReference, nullURI)
import Data.Maybe (fromMaybe)
import Modulus.BE.DB.Internal.Model (ConversationPublicID (ConversationPublicID))
import qualified Data.UUID as UUID
import Web.Hyperbole.Data.Encoded (encodedToText)

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

chatUrlNew :: URI
chatUrlNew = [relativeReference|/chat|]

chatUrl :: ConversationPublicID -> URI
chatUrl (ConversationPublicID cpID) =
  let str = "/chat/" <> UUID.toText cpID
   in fromMaybe nullURI $ parseRelativeReference (T.unpack str)


-- form but without col css
myForm :: (ViewAction (Action id)) => 
    Action id -> View (FormFields id) () -> View id ()
myForm a cnt = do
  vid <- context
  tag "form" @ onSubmit a $ do
    addContext (FormFields vid) cnt

myHyper ::
  forall id ctx.
  (ViewId id) => id -> View id () -> View ctx ()
myHyper vid vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) $
    addContext vid vw
