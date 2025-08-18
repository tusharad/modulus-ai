{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- AppError defination required for Application Monad Stack -}
module Modulus.BE.Monad.Error
  ( AppError (..)
  ) where

import Control.Exception
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics

-- | Application-specific error types
data AppError
  = DatabaseError Text
  | ValidationError Text
  | NotFoundError Text
  | AuthenticationError Text
  | AuthorizationError Text
  | ExternalServiceError Text
  | ConfigurationError Text
  | InternalError Text SomeException
  deriving (Show, Generic)

instance Exception AppError

instance ToJSON AppError where
  toJSON (DatabaseError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("database_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (ValidationError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("validation_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (NotFoundError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("not_found_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (AuthenticationError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("authentication_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (AuthorizationError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("authorization_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (ExternalServiceError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("external_service_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (ConfigurationError msg) =
    Aeson.object
      [ "error_type" Aeson..= ("configuration_error" :: Text)
      , "message" Aeson..= msg
      ]
  toJSON (InternalError msg _) =
    Aeson.object
      [ "error_type" Aeson..= ("internal_error" :: Text)
      , "message" Aeson..= msg
      ]
