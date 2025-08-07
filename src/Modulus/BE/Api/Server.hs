{-# LANGUAGE DataKinds #-}

module Modulus.BE.Api.Server
  ( appToServer
  ) where

import Modulus.BE.Api.V1
import Modulus.BE.Handler
import Modulus.BE.Monad.AppM
import Modulus.Common.Types
import Servant

serverV1 :: ServerT API_V1 AppM
serverV1 =
  authServer
    :<|> healthCheckHandler

appToServer :: AppConfig -> Application
appToServer cfg =
  serveWithContext
    (Proxy :: Proxy API_V1)
    (cfg :. EmptyContext)
    ( hoistServerWithContext
        (Proxy :: Proxy API_V1)
        (Proxy :: Proxy '[AppConfig])
        (appMToHandler cfg)
        serverV1
    )
