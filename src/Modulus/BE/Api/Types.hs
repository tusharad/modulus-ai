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
  , AddConversationRequest (..)
  , AddMessageRequest (..)
  , LLMRespStream (..)
  , LLMRespStreamBody (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Modulus.Common.Types (AuthTokens (..))
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
  } deriving (Generic, FromJSON, ToJSON)

data UserProfile = UserProfile
  { userProfileId :: UserID
  , userProfileEmail :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AddConversationRequest = AddConversationRequest {
    conversationTitle :: Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AddMessageRequest = AddMessageRequest {
    messageContent :: Text
  , addMessageRole :: Text
  , addMessageProvider :: Maybe Text
  , addMessageModel :: Maybe Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data LLMRespStream = LLMRespStream {
    respContent :: Text
  , promptToken :: Maybe Int
  , inputToken :: Maybe Int
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

data LLMRespStreamBody = LLMRespStreamBody {
    modelUsed :: Text,
    provider :: Text,
    apiKey :: Maybe Text
} deriving (Eq, Show, Generic, FromJSON, ToJSON)
