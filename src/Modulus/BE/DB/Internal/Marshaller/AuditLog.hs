{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.AuditLog
  ( -- * Audit Log Marshallers
    auditLogIDField
  , auditLogOrganizationIDField
  , auditLogUserIDField
  , auditLogActionField
  , auditLogDetailsField
  , auditLogIPAddressField
  , auditLogCreatedAtField
  , auditLogMarshaller
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.Organization
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Audit Log Fields
auditLogIDField :: FieldDefinition NotNull AuditLogID
auditLogIDField = coerceField $ bigSerialField "id"

auditLogOrganizationIDField :: FieldDefinition Nullable (Maybe OrganizationID)
auditLogOrganizationIDField = nullableField $ coerceField $ uuidField "organization_id"

auditLogUserIDField :: FieldDefinition Nullable (Maybe UserID)
auditLogUserIDField = nullableField $ coerceField $ uuidField "user_id"

auditLogActionField :: FieldDefinition NotNull Text
auditLogActionField = unboundedTextField "action"

auditLogDetailsField :: FieldDefinition Nullable (Maybe Text)
auditLogDetailsField = nullableField $ jsonbField "details"

auditLogIPAddressField :: FieldDefinition Nullable (Maybe Text)
auditLogIPAddressField = nullableField $ unboundedTextField "ip_address"

auditLogCreatedAtField :: FieldDefinition NotNull UTCTime
auditLogCreatedAtField = organizationCreatedAtField

-- Audit Log Marshaller
auditLogMarshaller :: SqlMarshaller AuditLogWrite AuditLogRead
auditLogMarshaller =
  AuditLog
    <$> marshallReadOnly (marshallField (\AuditLog {..} -> auditLogID) auditLogIDField)
    <*> marshallField
      (\AuditLog {..} -> auditLogOrganizationID)
      auditLogOrganizationIDField
    <*> marshallField (\AuditLog {..} -> auditLogUserID) auditLogUserIDField
    <*> marshallField (\AuditLog {..} -> auditLogAction) auditLogActionField
    <*> marshallField (\AuditLog {..} -> auditLogDetails) auditLogDetailsField
    <*> marshallField (\AuditLog {..} -> auditLogIPAddress) auditLogIPAddressField
    <*> marshallReadOnly
      (marshallField (\AuditLog {..} -> auditLogCreatedAt) auditLogCreatedAtField)
