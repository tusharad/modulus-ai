{- Exposing all Handler modules -}

module Modulus.BE.Handler
  ( healthCheckHandler
  , module Export
  ) where

import Modulus.BE.Handler.Auth as Export
import Modulus.BE.Monad.AppM (AppM)

healthCheckHandler :: AppM String
healthCheckHandler = return "Hello from Modulus backend!"
