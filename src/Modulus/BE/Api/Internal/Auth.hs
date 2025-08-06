{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.BE.Api.Internal.Auth
  ( AuthAPI
  ) where

import Modulus.BE.Api.Types
import Servant
import Data.Text (Text)
import Modulus.BE.Auth.JwtAuthCombinator 

type AuthAPI =
  "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] UserProfile
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthTokens
    :<|> "verify-otp" :> ReqBody '[JSON] OTPVerifyRequest :> Post '[JSON] Text
    :<|> WithJWTAuth :> "me" :> Get '[JSON] Text
