{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.User
  ( -- * User Marshallers
    userIDField
  , userEmailField
  , userHashedPasswordField
  , userLastLoginAtField
  , userCreatedAtField
  , userUpdatedAtField
  , userIsEmailVerifiedField
  , userMarshaller

    -- * User Role Marshaller
  , userRoleField
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Int (Int32)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils
import Orville.PostgreSQL

-- User Fields
userIDField :: FieldDefinition NotNull UserID
userIDField =
  setDefaultValue
    (coerceDefaultValue genRandomUuidDefault)
    $ coerceField
    $ uuidField "id"

userEmailField :: FieldDefinition NotNull Text
userEmailField = unboundedTextField "email"

userHashedPasswordField :: FieldDefinition NotNull Text
userHashedPasswordField = unboundedTextField "hashed_password"

userLastLoginAtField :: FieldDefinition Nullable (Maybe UTCTime)
userLastLoginAtField = nullableField $ utcTimestampField "last_login_at"

userCreatedAtField :: FieldDefinition NotNull UTCTime
userCreatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "created_at"

userUpdatedAtField :: FieldDefinition NotNull UTCTime
userUpdatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "updated_at"

userIsEmailVerifiedField :: FieldDefinition NotNull Bool
userIsEmailVerifiedField =
  setDefaultValue (booleanDefault False) $ booleanField "is_email_verified"

userRoleSqlType :: SqlType UserRole
userRoleSqlType =
  tryConvertSqlType
    convertRoleToString
    convertStringToRole
    unboundedText
  where
    convertRoleToString :: UserRole -> Text
    convertRoleToString UserRoleAdmin = "admin"
    convertRoleToString UserRoleMember = "member"

    convertStringToRole :: Text -> Either String UserRole
    convertStringToRole "admin" = Right UserRoleAdmin
    convertStringToRole "member" = Right UserRoleMember
    convertStringToRole s = Left $ "Invalid UserRole value: " ++ T.unpack s

userRoleFieldFunc :: String -> FieldDefinition NotNull UserRole
userRoleFieldFunc = fieldOfType userRoleSqlType

-- User Role Field
userRoleField :: FieldDefinition NotNull UserRole
userRoleField = userRoleFieldFunc "role"

userFailedLoginAttemptsField :: FieldDefinition NotNull Int32
userFailedLoginAttemptsField =
  setDefaultValue (integerDefault 0) $ integerField "failed_login_attempts"

userLockedUntilField :: FieldDefinition Nullable (Maybe UTCTime)
userLockedUntilField = nullableField $ utcTimestampField "locked_until"

-- User Marshaller
userMarshaller :: SqlMarshaller UserWrite UserRead
userMarshaller =
  User
    <$> marshallReadOnly (marshallField (\User {..} -> userID) userIDField)
    <*> marshallField (\User {..} -> userEmail) userEmailField
    <*> marshallField
      (\User {..} -> userHashedPassword)
      userHashedPasswordField
    <*> marshallField (\User {..} -> userLastLoginAt) userLastLoginAtField
    <*> marshallReadOnly
      (marshallField (\User {..} -> userCreatedAt) userCreatedAtField)
    <*> marshallReadOnly
      (marshallField (\User {..} -> userUpdatedAt) userUpdatedAtField)
    <*> marshallField
      (\User {..} -> userIsEmailVerified)
      userIsEmailVerifiedField
    <*> marshallField
      (\User {..} -> userFailedLoginAttempts)
      userFailedLoginAttemptsField
    <*> marshallField
      (\User {..} -> userLockedUntil)
      userLockedUntilField
