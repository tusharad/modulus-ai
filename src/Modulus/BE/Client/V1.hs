{-# LANGUAGE DataKinds #-}

module Modulus.BE.Client.V1
  ( registerClient
  , loginClient
  , verifyOTPClient
  , meClient
  , healthCheckClient
  , refreshTokenClient
  -- * Exporting some handler function for FE. 
  -- FE should only use this module for interacting with BE
  , registerHandler
  , loginHandler
  , verifyOTPHandler
  ) where

import Data.Text (Text)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Servant
import Servant.Client
import Modulus.BE.Handler (registerHandler, loginHandler, verifyOTPHandler)

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
