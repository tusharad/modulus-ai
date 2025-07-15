{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module App.Common.Utils
  ( homeUrl
  , relUrl
  , myHyper
  , myForm
  , inputFile
  , select
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI
import Network.URI.Static
import Web.Hyperbole
import Web.Hyperbole.Data.Encoded (encodedToText)
import Web.Hyperbole.HyperView
import Web.Hyperbole.HyperView.Forms

homeUrl :: URI
homeUrl = [relativeReference|/|]

notFoundUrl :: URI
notFoundUrl = [relativeReference|/not-found|]

relUrl :: Text -> URI
relUrl x = fromMaybe notFoundUrl (parseRelativeReference (T.unpack x))

-- form but without col css
myForm :: (ViewAction (Action id)) => Action id -> View (FormFields id) () -> View id ()
myForm a cnt = do
  vid <- context
  tag "form" @ onSubmit a $ do
    addContext (FormFields vid) cnt

-- hyper but without col css
myHyper ::
  forall id ctx.
  (ViewId id) => id -> View id () -> View ctx ()
myHyper vid vw = do
  tag "div" @ att "id" (encodedToText $ toViewId vid) $
    addContext vid vw

inputFile :: View (Input id a) ()
inputFile = do
  Input (FieldName nm) <- context
  tag "input" @ att "type" "file" . name nm $ none

select :: View (Input id a) () -> View (Input id a) ()
select x = do
  Input (FieldName nm) <- context
  tag "select" @ name nm $ x
