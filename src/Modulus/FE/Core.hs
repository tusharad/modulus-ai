{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.FE.Core (app) where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Effectful
import Modulus.FE.Common.Types
import Modulus.FE.Effects.StateStore
import qualified Modulus.FE.Page.Chat as Chat
import Web.Hyperbole

toDocument :: BL.ByteString -> BL.ByteString
toDocument cnt =
  [i|<html>
      <head>
        <meta httpEquiv="Content-Type" content="text/html" charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <script type="text/javascript">#{scriptEmbed}</script>
        <style type="text/css">#{cssResetEmbed}</style>
        <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
      </head>
      <body>#{cnt}</body>
    </html>|]

router :: (Hyperbole :> es) => AppRoute -> Eff es Response
router r = do
  case r of
    Main -> redirect (routeUrl $ Chat Nothing)
    Chat mbChatId -> runPage $ Chat.page (fromMaybe 0 mbChatId)

app :: StateStoreMap -> Application
app stateMap =
  liveApp
    toDocument
    (runM . routeRequest $ router)
  where
    runM :: (IOE :> es, Hyperbole :> es) => Eff (StateStoreEff : es) a -> Eff es a
    runM = runStateStoreIO stateMap
