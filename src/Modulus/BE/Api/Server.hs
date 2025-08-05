module Modulus.BE.Api.Server
  ( appToServer
  ) where

import Modulus.BE.Api.V1
import Modulus.BE.Monad.AppM
import Servant
import Modulus.BE.Handler

serverV1 :: ServerT API_V1 AppM
serverV1 = authServer
    :<|> healthCheckHandler 

appToServer :: AppConfig -> Application
appToServer cfg =
  serve
    (Proxy :: Proxy API_V1)
    ( hoistServer
        (Proxy :: Proxy API_V1)
        (appMToHandler cfg)
        serverV1
    )
