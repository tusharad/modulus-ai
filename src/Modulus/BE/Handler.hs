{- Exposing all Handler modules -}

module Modulus.BE.Handler (
    healthCheckHandler
  , module Export 
 ) where

import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Handler.Auth as Export

healthCheckHandler :: AppM String
healthCheckHandler = return "Hello from Modulus backend!"
