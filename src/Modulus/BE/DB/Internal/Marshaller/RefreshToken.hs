{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.RefreshToken
  ( -- * Refresh tokens marshaller
    refreshTokenMarshaller
  , refreshTokenUserIDField
  , refreshTokenIDField
  , refreshTokenTokenHashField
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils (genRandomUuidDefault)
import Orville.PostgreSQL

refreshTokenIDField :: FieldDefinition NotNull RefreshTokenID
refreshTokenIDField = coerceField $ setDefaultValue genRandomUuidDefault $ uuidField "id"

refreshTokenUserIDField :: FieldDefinition NotNull UserID
refreshTokenUserIDField = coerceField $ uuidField "user_id"

refreshTokenTokenHashField :: FieldDefinition NotNull Text
refreshTokenTokenHashField = boundedTextField "token_hash" 255

refreshTokenExpiresAtField :: FieldDefinition NotNull UTCTime
refreshTokenExpiresAtField = utcTimestampField "expires_at"

refreshTokenCreatedAtField :: FieldDefinition NotNull UTCTime
refreshTokenCreatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "created_at"

refreshTokenIsRevokedField :: FieldDefinition NotNull Bool
refreshTokenIsRevokedField =
  setDefaultValue (booleanDefault False) $ booleanField "is_revoked"

-- MARSHALLERS
refreshTokenMarshaller :: SqlMarshaller RefreshTokenWrite RefreshTokenRead
refreshTokenMarshaller =
  RefreshToken
    <$> marshallReadOnly
      (marshallField (\RefreshToken {..} -> refreshTokenID) refreshTokenIDField)
    <*> marshallField (\RefreshToken {..} -> refreshTokenUserID) refreshTokenUserIDField
    <*> marshallField
      (\RefreshToken {..} -> refreshTokenTokenHash)
      refreshTokenTokenHashField
    <*> marshallField
      (\RefreshToken {..} -> refreshTokenExpiresAt)
      refreshTokenExpiresAtField
    <*> marshallReadOnly
      ( marshallField
          (\RefreshToken {..} -> refreshTokenCreatedAt)
          refreshTokenCreatedAtField
      )
    <*> marshallField
      (\RefreshToken {..} -> refreshTokenIsRevoked)
      refreshTokenIsRevokedField
