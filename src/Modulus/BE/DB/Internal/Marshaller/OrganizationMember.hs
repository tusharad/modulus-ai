{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.OrganizationMember
  ( -- * Organization Member Marshallers
    organizationMemberOrganizationIDField
  , organizationMemberUserIDField
  , organizationMemberRoleField
  , organizationMemberCreatedAtField
  , organizationMemberMarshaller
  ) where

import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.Organization (organizationCreatedAtField)
import Modulus.BE.DB.Internal.Marshaller.User (userRoleField)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Organization Member Fields
organizationMemberOrganizationIDField :: FieldDefinition NotNull OrganizationID
organizationMemberOrganizationIDField = coerceField $ uuidField "organization_id"

organizationMemberUserIDField :: FieldDefinition NotNull UserID
organizationMemberUserIDField = coerceField $ uuidField "user_id"

organizationMemberRoleField :: FieldDefinition NotNull UserRole
organizationMemberRoleField = userRoleField

organizationMemberCreatedAtField :: FieldDefinition NotNull UTCTime
organizationMemberCreatedAtField = organizationCreatedAtField

-- Organization Member Marshaller
organizationMemberMarshaller ::
  SqlMarshaller OrganizationMemberWrite OrganizationMemberRead
organizationMemberMarshaller =
  OrganizationMember
    <$> marshallField
      (\OrganizationMember {..} -> organizationMemberOrganizationID)
      organizationMemberOrganizationIDField
    <*> marshallField
      (\OrganizationMember {..} -> organizationMemberUserID)
      organizationMemberUserIDField
    <*> marshallField
      (\OrganizationMember {..} -> organizationMemberRole)
      organizationMemberRoleField
    <*> marshallReadOnly
      ( marshallField
          (\OrganizationMember {..} -> organizationMemberCreatedAt)
          organizationMemberCreatedAtField
      )
