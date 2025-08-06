{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Modulus.BE.Api.Types
  ( RegisterRequest (..)
  , LoginRequest (..)
  , UserProfile (..)
  , AuthTokens (..)
  , OTPVerifyRequest (..)
  , RefreshTokenRequest (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Modulus.BE.DB.Internal.Model

data RegisterRequest = RegisterRequest
  { registerEmail :: Text
  , registerPassword :: Text
  , registerConfirmPassword :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LoginRequest = LoginRequest
  { loginEmail :: Text
  , loginPassword :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OTPVerifyRequest = OTPVerifyRequest
  { verifyEmail :: Text
  , verifyOTP :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RefreshTokenRequest = RefreshTokenRequest
  { refreshToken :: Text
  } deriving (Generic, FromJSON)

data UserProfile = UserProfile
  { userProfileId :: UserID
  , userProfileEmail :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AuthTokens = AuthTokens
  { accessToken :: Text
  , refreshToken :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
