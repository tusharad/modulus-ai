{-# LANGUAGE DataKinds #-}

module Modulus.BE.Client.V1
  ( registerClient
  , loginClient
  , verifyOTPClient
  , meClient
  , healthCheckClient
  , refreshTokenClient
  ) where

import Data.Text (Text)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Servant
import Servant.Client

registerClient :: RegisterRequest -> ClientM UserProfile
loginClient :: LoginRequest -> ClientM AuthTokens
verifyOTPClient :: OTPVerifyRequest -> ClientM Text
refreshTokenClient :: RefreshTokenRequest -> ClientM AuthTokens
meClient :: Text -> ClientM Text
healthCheckClient :: ClientM String
( registerClient
    :<|> loginClient
    :<|> verifyOTPClient
    :<|> refreshTokenClient
    :<|> meClient
  )
  :<|> healthCheckClient = client (Proxy :: Proxy API_V1)
