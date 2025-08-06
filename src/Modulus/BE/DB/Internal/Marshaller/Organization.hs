{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.Organization
  ( -- * Organization Marshallers
    organizationIDField
  , organizationNameField
  , organizationCreatedAtField
  , organizationUpdatedAtField
  , organizationMarshaller
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Utils
import Orville.PostgreSQL

-- Organization Marshaller
organizationMarshaller :: SqlMarshaller OrganizationWrite OrganizationRead
organizationMarshaller =
  Organization
    <$> marshallReadOnly
      (marshallField (\Organization {..} -> organizationID) organizationIDField)
    <*> marshallField (\Organization {..} -> organizationName) organizationNameField
    <*> marshallReadOnly
      ( marshallField
          (\Organization {..} -> organizationCreatedAt)
          organizationCreatedAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\Organization {..} -> organizationUpdatedAt)
          organizationUpdatedAtField
      )

-- Organization Fields
organizationIDField :: FieldDefinition NotNull OrganizationID
organizationIDField = coerceField $ setDefaultValue genRandomUuidDefault $ uuidField "id"

organizationNameField :: FieldDefinition NotNull Text
organizationNameField = unboundedTextField "name"

organizationCreatedAtField :: FieldDefinition NotNull UTCTime
organizationCreatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "created_at"

organizationUpdatedAtField :: FieldDefinition NotNull UTCTime
organizationUpdatedAtField =
  setDefaultValue
    currentUTCTimestampDefault
    $ utcTimestampField "updated_at"
