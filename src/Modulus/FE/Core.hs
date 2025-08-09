{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.FE.Core (app) where

import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate (i)
import Effectful
import Modulus.Common.Types
import Modulus.FE.Common.Types
import Modulus.FE.Effects.AppConfig
import Modulus.FE.Effects.StateStore
import qualified Modulus.FE.Page.Chat as Chat
import qualified Modulus.FE.Page.Login as Login
import qualified Modulus.FE.Page.Register as Register
import qualified Modulus.FE.Page.Verify as Verify
import Web.Hyperbole

toDocument :: BL.ByteString -> BL.ByteString
toDocument cnt =
  [i|<html>
      <head>
        <meta httpEquiv="Content-Type" content="text/html" charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <script type="text/javascript">#{scriptEmbed}</script>
        <style type="text/css">#{cssResetEmbed}</style>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet" xintegrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">

    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap" rel="stylesheet">

      </head>
      <body>#{cnt}</body>
       <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" xintegrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>

    </html>|]

router ::
  (Hyperbole :> es, IOE :> es, AppConfigEff :> es, StateStoreEff :> es) =>
  AppRoute -> Eff es Response
router r = do
  case r of
    Main -> redirect (routeUri $ Chat Nothing)
    Register -> runPage Register.page
    Login -> runPage Login.page
    Verify -> runPage Verify.page
    Chat mbChatId -> runPage $ Chat.page mbChatId

app :: StateStoreMap -> StateStoreData -> AppConfig -> Application
app stateMap stData appCfg =
  liveApp
    toDocument
    (runM . routeRequest $ router)
  where
    runM ::
      (IOE :> es, Hyperbole :> es) =>
      Eff (StateStoreEff : AppConfigEff : es) a -> Eff es a
    runM = runAppConfigIO appCfg . runStateStoreIO stateMap stData
