{-# LANGUAGE DataKinds #-}

module Modulus.BE.Client.V1
  ( {-
      registerClient
    , loginClient
    , verifyOTPClient
    , meClient
    , healthCheckClient
    , refreshTokenClient
    , addConversationClient
    , getConversationsClient
    -}

    -- * Exporting some handler function for FE.

  -- FE should only use this module for interacting with BE
    registerHandler
  , loginHandler
  , verifyOTPHandler
  , meHandler
  , authenticateToken
  , getConversationsHandler
  ) where

import Modulus.BE.Handler 
import Modulus.BE.Auth.JwtAuthCombinator (authenticateToken)
import Modulus.BE.Handler.Conversations

{-
import Data.Text (Text)
import Modulus.BE.Api.Types
import Modulus.BE.Api.V1
import Servant
import Servant.Client
import Modulus.BE.DB.Internal.Model (ConversationPublicID, ConversationRead)

registerClient :: RegisterRequest -> ClientM UserProfile
loginClient :: LoginRequest -> ClientM AuthTokens
verifyOTPClient :: OTPVerifyRequest -> ClientM Text
refreshTokenClient :: RefreshTokenRequest -> ClientM AuthTokens
meClient :: Text -> ClientM Text
addConversationClient :: Text -> AddConversationRequest -> ClientM ConversationPublicID
getConversationsClient :: Text -> ClientM [ConversationRead]
healthCheckClient :: ClientM String
( registerClient
    :<|> loginClient
    :<|> verifyOTPClient
    :<|> refreshTokenClient
    :<|> meClient
  )
  :<|> ( addConversationClient
           :<|> getConversationsClient
         )
  :<|> healthCheckClient = client (Proxy :: Proxy API_V1)
  -}
