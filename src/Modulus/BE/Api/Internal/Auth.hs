{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.BE.Api.Internal.Auth
  ( AuthAPI
  ) where

import Modulus.BE.Api.Types
import Servant

type AuthAPI =
  "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] UserProfile
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthTokens
