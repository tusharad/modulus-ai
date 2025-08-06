{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.EmailVerificationOTP
  ( -- * Email Verification OTP Marshallers
    emailVerificationOTPIDField
  , emailVerificationOTPUserIDField
  , emailVerificationOTPOtpHashField
  , emailVerificationOTPExpiresAtField
  , emailVerificationOTPCreatedAtField
  , emailVerificationOTPMarshaller
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Modulus.BE.DB.Internal.Marshaller.Organization
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils (genRandomUuidDefault)
import Orville.PostgreSQL

-- Email Verification OTP Fields
emailVerificationOTPIDField :: FieldDefinition NotNull UUID
emailVerificationOTPIDField = setDefaultValue genRandomUuidDefault $ uuidField "id"

emailVerificationOTPUserIDField :: FieldDefinition NotNull UserID
emailVerificationOTPUserIDField = coerceField $ uuidField "user_id"

emailVerificationOTPOtpHashField :: FieldDefinition NotNull Text
emailVerificationOTPOtpHashField = unboundedTextField "otp_hash"

emailVerificationOTPExpiresAtField :: FieldDefinition NotNull UTCTime
emailVerificationOTPExpiresAtField = utcTimestampField "expires_at"

emailVerificationOTPCreatedAtField :: FieldDefinition NotNull UTCTime
emailVerificationOTPCreatedAtField = organizationCreatedAtField

-- Email Verification OTP Marshaller
emailVerificationOTPMarshaller ::
  SqlMarshaller EmailVerificationOTPWrite EmailVerificationOTPRead
emailVerificationOTPMarshaller =
  EmailVerificationOTP
    <$> marshallReadOnly
      ( marshallField
          (\EmailVerificationOTP {..} -> emailVerificationOTPID)
          emailVerificationOTPIDField
      )
    <*> marshallField
      (\EmailVerificationOTP {..} -> emailVerificationOTPUserID)
      emailVerificationOTPUserIDField
    <*> marshallField
      (\EmailVerificationOTP {..} -> emailVerificationOTPOtpHash)
      emailVerificationOTPOtpHashField
    <*> marshallField
      (\EmailVerificationOTP {..} -> emailVerificationOTPExpiresAt)
      emailVerificationOTPExpiresAtField
    <*> marshallReadOnly
      ( marshallField
          (\EmailVerificationOTP {..} -> emailVerificationOTPCreatedAt)
          emailVerificationOTPCreatedAtField
      )
