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
import Modulus.BE.DB.Internal.Model
import Modulus.Common.Types (AuthTokens (..))
import Servant.Multipart

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

newtype RefreshTokenRequest = RefreshTokenRequest
  { refreshToken :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data UserProfile = UserProfile
  { userProfileId :: UserID
  , userProfileEmail :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype AddConversationRequest = AddConversationRequest
  { conversationTitle :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AddMessageRequest = AddMessageRequest
  { messageContent :: Text
  , addMessageRole :: Text
  , addMessageProvider :: Maybe Text
  , addMessageModel :: Maybe Text
  , addMessageAttachment :: Maybe (FileData Mem)
  }
  deriving (Eq, Show, Generic)

instance FromMultipart Mem AddMessageRequest where
  fromMultipart multipartData =
    AddMessageRequest
      <$> lookupInput "messageContent" multipartData
      <*> lookupInput "addMessageRole" multipartData
      <*> optionalInput "addMessageProvider" multipartData
      <*> optionalInput "addMessageModel" multipartData
      <*> optionalInputFile "addMessageAttachment" multipartData

optionalInputFile ::
  Text -> MultipartData tag -> Either String (Maybe (FileData tag))
optionalInputFile name md = Right $ hush $ lookupFile name md

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

optionalInput :: Text -> MultipartData tag -> Either String (Maybe Text)
optionalInput name md = Right $ hush $ lookupInput name md

data LLMRespStream = LLMRespStream
  { respContent :: Text
  , promptToken :: Maybe Int
  , inputToken :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data LLMRespStreamBody = LLMRespStreamBody
  { modelUsed :: Text
  , provider :: Text
  , apiKey :: Maybe Text
  , toolCall :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
