{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.BE.Api.V1
  ( API_V1
  ) where

import Modulus.BE.Api.Internal.Auth (AuthAPI)
import Servant

type API_V1 =
  "api" :> V1

type V1 = "v1" :> API 

type API = "auth" :> AuthAPI
            :<|> "health-check" :> Get '[JSON] String
