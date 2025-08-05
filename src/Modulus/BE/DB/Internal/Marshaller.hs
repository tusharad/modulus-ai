{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Modulus.BE.DB.Internal.Marshaller
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module provides the Orville 'SqlMarshaller' definitions for the data
types defined in "Modulus.BE.DB.Internal.Model". Marshallers are responsible for
mapping between the Haskell record fields and the corresponding database
columns, handling data type conversions and default values.

== Purpose ==

Each marshaller defines how a specific Haskell data type (e.g., 'User',
'Conversation') is serialized to and deserialized from rows in the database.
They specify which fields correspond to which columns, how to handle
read-only fields (like auto-generated IDs and timestamps), and how to
convert between Haskell types and PostgreSQL types.

== Architectural Role ==

This module is an internal component of the database layer. It is consumed
by "Modulus.BE.DB.Internal.Table" to define the structure of database tables and by
query execution logic within other `Modulus.BE.DB.Internal.*` modules to read from and
write to the database.

== Design Notes ==

*   **Field Definitions**: Individual 'FieldDefinition's are created for
    each database column. These definitions include the column name, SQL
    type, nullability, and any default values. Default values set here
    (e.g., using 'setDefaultValue') inform Orville's schema management.
*   **Marshaller Construction**: Marshallers are built using Orville's
    'Applicative' syntax. Functions like 'marshallField' and
    'marshallReadOnly' are composed to map record fields to database columns.
*   **Read-Only Fields**: Fields populated by the database (e.g., 'id',
    'created_at') are typically wrapped with 'marshallReadOnly' to indicate
    they should not be included in INSERT or UPDATE statements.
*   **Reusability**: Common field definitions (e.g., 'userIDField') are
    often reused across multiple marshallers where the underlying column
    structure is the same, promoting consistency.
*   **Enum Handling**: Haskell ADTs representing database ENUMs (e.g.,
    'UserRole') are marshalled using 'convertFieldDefinition' to map between
    the Haskell constructors and the corresponding string values in the
    database.
-}
--TODO: Breakup Marshaller in Per Table Module
module Modulus.BE.DB.Internal.Marshaller
  ( -- * Organization Marshallers
    organizationIDField
  , organizationNameField
  , organizationCreatedAtField
  , organizationUpdatedAtField
  , organizationMarshaller

    -- * User Marshallers
  , userIDField
  , userEmailField
  , userHashedPasswordField
  , userLastLoginAtField
  , userCreatedAtField
  , userUpdatedAtField
  , userIsEmailVerifiedField
  , userMarshaller

    -- * User Role Marshaller
  , userRoleField

    -- * Organization Member Marshallers
  , organizationMemberOrganizationIDField
  , organizationMemberUserIDField
  , organizationMemberRoleField
  , organizationMemberCreatedAtField
  , organizationMemberMarshaller

    -- * Conversation Marshallers
  , conversationIDField
  , conversationPublicIDField
  , conversationOrganizationIDField
  , conversationUserIDField
  , conversationTitleField
  , conversationCreatedAtField
  , conversationUpdatedAtField
  , conversationMarshaller

    -- * Message Role Marshaller
  , messageRoleField

    -- * Chat Message Marshallers
  , chatMessageIDField
  , chatMessagePublicIDField
  , chatMessageConversationIDField
  , chatMessageOrganizationIDField
  , chatMessageRoleField
  , chatMessageContentField
  , chatMessageModelUsedField
  , chatMessageCreatedAtField
  , chatMessageMarshaller

    -- * Message Attachment Marshallers
  , messageAttachmentIDField
  , messageAttachmentMessageIDField
  , messageAttachmentOrganizationIDField
  , messageAttachmentFileNameField
  , messageAttachmentFileTypeField
  , messageAttachmentFileSizeBytesField
  , messageAttachmentStoragePathField
  , messageAttachmentCreatedAtField
  , messageAttachmentMarshaller

    -- * Subscription Plan Marshallers
  , subscriptionPlanIDField
  , subscriptionPlanNameField
  , subscriptionPlanPriceCentsField
  , subscriptionPlanFeaturesField
  , subscriptionPlanMarshaller

    -- * Subscription Status Marshaller
  , subscriptionStatusField

    -- * User Subscription Marshallers
  , userSubscriptionIDField
  , userSubscriptionOrganizationIDField
  , userSubscriptionPlanIDField
  , userSubscriptionStripeSubscriptionIDField
  , userSubscriptionCurrentPeriodEndsAtField
  , userSubscriptionCreatedAtField
  , userSubscriptionUpdatedAtField
  , userSubscriptionMarshaller

    -- * Audit Log Marshallers
  , auditLogIDField
  , auditLogOrganizationIDField
  , auditLogUserIDField
  , auditLogActionField
  , auditLogDetailsField
  , auditLogIPAddressField
  , auditLogCreatedAtField
  , auditLogMarshaller

    -- * Email Verification OTP Marshallers
  , emailVerificationOTPIDField
  , emailVerificationOTPUserIDField
  , emailVerificationOTPOtpHashField
  , emailVerificationOTPExpiresAtField
  , emailVerificationOTPCreatedAtField
  , emailVerificationOTPMarshaller

    -- * Refresh tokens marshaller
  , refreshTokenMarshaller
  , refreshTokenUserIDField
  , refreshTokenIDField
  , refreshTokenTokenHashField
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32, Int64)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL
import qualified Orville.PostgreSQL.Expr as Expr

genRandomUuidDefault :: DefaultValue UUID
genRandomUuidDefault =
  rawSqlDefault $
    Expr.functionCall (Expr.functionName "gen_random_uuid") []

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

-- Organization Member Fields
organizationMemberOrganizationIDField :: FieldDefinition NotNull OrganizationID
organizationMemberOrganizationIDField = coerceField $ uuidField "organization_id"

organizationMemberUserIDField :: FieldDefinition NotNull UserID
organizationMemberUserIDField = coerceField $ uuidField "user_id"

organizationMemberRoleField :: FieldDefinition NotNull UserRole
organizationMemberRoleField = userRoleField

organizationMemberCreatedAtField :: FieldDefinition NotNull UTCTime
organizationMemberCreatedAtField = organizationCreatedAtField

-- Conversation Fields
conversationIDField :: FieldDefinition NotNull ConversationID
conversationIDField = coerceField $ bigSerialField "id"

conversationPublicIDField :: FieldDefinition NotNull ConversationPublicID
conversationPublicIDField = coerceField $ 
    setDefaultValue genRandomUuidDefault $ uuidField "public_id"

conversationOrganizationIDField :: FieldDefinition NotNull OrganizationID
conversationOrganizationIDField = coerceField $ uuidField "organization_id"

conversationUserIDField :: FieldDefinition Nullable (Maybe UserID)
conversationUserIDField = nullableField $ coerceField $ uuidField "user_id"

conversationTitleField :: FieldDefinition NotNull Text
conversationTitleField = unboundedTextField "title"

conversationCreatedAtField :: FieldDefinition NotNull UTCTime
conversationCreatedAtField = organizationCreatedAtField

conversationUpdatedAtField :: FieldDefinition NotNull UTCTime
conversationUpdatedAtField = organizationUpdatedAtField

messageRoleSqlType :: SqlType MessageRole
messageRoleSqlType =
  tryConvertSqlType convertRoleToString convertStringToRole unboundedText
  where
    convertRoleToString :: MessageRole -> Text
    convertRoleToString MessageRoleUser = "user"
    convertRoleToString MessageRoleAssistant = "assistant"
    convertRoleToString MessageRoleSystem = "system"
    convertRoleToString MessageRoleTool = "tool"

    convertStringToRole :: Text -> Either String MessageRole
    convertStringToRole "user" = Right MessageRoleUser
    convertStringToRole "assistant" = Right MessageRoleAssistant
    convertStringToRole "system" = Right MessageRoleSystem
    convertStringToRole "tool" = Right MessageRoleTool
    convertStringToRole s = Left $ "Invalid MessageRole value: " ++ T.unpack s

messageRoleFieldFunc :: String -> FieldDefinition NotNull MessageRole
messageRoleFieldFunc = fieldOfType messageRoleSqlType

-- Message Role Field
messageRoleField :: FieldDefinition NotNull MessageRole
messageRoleField = messageRoleFieldFunc "role"

-- Chat Message Fields
chatMessageIDField :: FieldDefinition NotNull ChatMessageID
chatMessageIDField = coerceField $ bigSerialField "id"

chatMessagePublicIDField :: FieldDefinition NotNull ChatMessagePublicID
chatMessagePublicIDField = coerceField $ 
    setDefaultValue genRandomUuidDefault $ uuidField "public_id"

chatMessageConversationIDField :: FieldDefinition NotNull ConversationID
chatMessageConversationIDField = coerceField $ bigIntegerField "conversation_id"

chatMessageOrganizationIDField :: FieldDefinition NotNull OrganizationID
chatMessageOrganizationIDField = coerceField $ uuidField "organization_id"

chatMessageRoleField :: FieldDefinition NotNull MessageRole
chatMessageRoleField = messageRoleField

chatMessageContentField :: FieldDefinition NotNull Text
chatMessageContentField = unboundedTextField "content"

chatMessageModelUsedField :: FieldDefinition Nullable (Maybe Text)
chatMessageModelUsedField = nullableField $ unboundedTextField "model_used"

chatMessageCreatedAtField :: FieldDefinition NotNull UTCTime
chatMessageCreatedAtField = organizationCreatedAtField

-- Message Attachment Fields
messageAttachmentIDField :: FieldDefinition NotNull MessageAttachmentID
messageAttachmentIDField = coerceField $ bigSerialField "id"

messageAttachmentMessageIDField :: FieldDefinition NotNull ChatMessageID
messageAttachmentMessageIDField = coerceField $ bigIntegerField "message_id"

messageAttachmentOrganizationIDField :: FieldDefinition NotNull OrganizationID
messageAttachmentOrganizationIDField = coerceField $ uuidField "organization_id"

messageAttachmentFileNameField :: FieldDefinition NotNull Text
messageAttachmentFileNameField = unboundedTextField "file_name"

messageAttachmentFileTypeField :: FieldDefinition NotNull Text
messageAttachmentFileTypeField = unboundedTextField "file_type"

messageAttachmentFileSizeBytesField :: FieldDefinition NotNull Int64
messageAttachmentFileSizeBytesField = bigIntegerField "file_size_bytes"

messageAttachmentStoragePathField :: FieldDefinition NotNull Text
messageAttachmentStoragePathField = unboundedTextField "storage_path"

messageAttachmentCreatedAtField :: FieldDefinition NotNull UTCTime
messageAttachmentCreatedAtField = organizationCreatedAtField

-- Subscription Plan Fields
subscriptionPlanIDField :: FieldDefinition NotNull SubscriptionPlanID
subscriptionPlanIDField = coerceField $ boundedTextField "id" 255

subscriptionPlanNameField :: FieldDefinition NotNull Text
subscriptionPlanNameField = unboundedTextField "name"

subscriptionPlanPriceCentsField :: FieldDefinition NotNull Int32
subscriptionPlanPriceCentsField = integerField "price_cents"

subscriptionPlanFeaturesField :: FieldDefinition Nullable (Maybe Text)
subscriptionPlanFeaturesField = nullableField $ jsonbField "features"

subscriptionStatusSqlType :: SqlType SubscriptionStatus
subscriptionStatusSqlType =
  tryConvertSqlType convertStatusToString convertStringToStatus unboundedText
  where
    convertStatusToString :: SubscriptionStatus -> Text
    convertStatusToString SubscriptionStatusActive = "active"
    convertStatusToString SubscriptionStatusPastDue = "past_due"
    convertStatusToString SubscriptionStatusCanceled = "canceled"
    convertStatusToString SubscriptionStatusTrialing = "trialing"

    convertStringToStatus :: Text -> Either String SubscriptionStatus
    convertStringToStatus "active" = Right SubscriptionStatusActive
    convertStringToStatus "past_due" = Right SubscriptionStatusPastDue
    convertStringToStatus "canceled" = Right SubscriptionStatusCanceled
    convertStringToStatus "trialing" = Right SubscriptionStatusTrialing
    convertStringToStatus s = Left $ "Invalid SubscriptionStatus value: " ++ T.unpack s

subscriptionStatusFieldFunc :: String -> FieldDefinition NotNull SubscriptionStatus
subscriptionStatusFieldFunc = fieldOfType subscriptionStatusSqlType

-- Subscription Status Field
subscriptionStatusField :: FieldDefinition NotNull SubscriptionStatus
subscriptionStatusField = subscriptionStatusFieldFunc "status"

-- User Subscription Fields
userSubscriptionIDField :: FieldDefinition NotNull UserSubscriptionID
userSubscriptionIDField = coerceField $ setDefaultValue genRandomUuidDefault $ uuidField "id"

userSubscriptionOrganizationIDField :: FieldDefinition NotNull OrganizationID
userSubscriptionOrganizationIDField = coerceField $ uuidField "organization_id"

userSubscriptionPlanIDField :: FieldDefinition NotNull SubscriptionPlanID
userSubscriptionPlanIDField = coerceField $ boundedTextField "plan_id" 255

userSubscriptionStripeSubscriptionIDField :: FieldDefinition Nullable (Maybe Text)
userSubscriptionStripeSubscriptionIDField =
  nullableField $ unboundedTextField "stripe_subscription_id"

userSubscriptionCurrentPeriodEndsAtField :: FieldDefinition Nullable (Maybe UTCTime)
userSubscriptionCurrentPeriodEndsAtField =
  nullableField $ utcTimestampField "current_period_ends_at"

userSubscriptionCreatedAtField :: FieldDefinition NotNull UTCTime
userSubscriptionCreatedAtField = organizationCreatedAtField

userSubscriptionUpdatedAtField :: FieldDefinition NotNull UTCTime
userSubscriptionUpdatedAtField = organizationUpdatedAtField

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

userFailedLoginAttemptsField :: FieldDefinition NotNull Int32
userFailedLoginAttemptsField =
  setDefaultValue (integerDefault 0) $ integerField "failed_login_attempts"

userLockedUntilField :: FieldDefinition Nullable (Maybe UTCTime)
userLockedUntilField = nullableField $ utcTimestampField "locked_until"

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

-- Conversation Marshaller
conversationMarshaller :: SqlMarshaller ConversationWrite ConversationRead
conversationMarshaller =
  Conversation
    <$> marshallReadOnly
      (marshallField (\Conversation {..} -> conversationID) conversationIDField)
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationPublicID)
          conversationPublicIDField
      )
    <*> marshallField
      (\Conversation {..} -> conversationOrganizationID)
      conversationOrganizationIDField
    <*> marshallField
      (\Conversation {..} -> conversationUserID)
      conversationUserIDField
    <*> marshallField (\Conversation {..} -> conversationTitle) conversationTitleField
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationCreatedAt)
          conversationCreatedAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\Conversation {..} -> conversationUpdatedAt)
          conversationUpdatedAtField
      )

-- Chat Message Marshaller
chatMessageMarshaller :: SqlMarshaller ChatMessageWrite ChatMessageRead
chatMessageMarshaller =
  ChatMessage
    <$> marshallReadOnly
      (marshallField (\ChatMessage {..} -> chatMessageID) chatMessageIDField)
    <*> marshallReadOnly
      ( marshallField
          (\ChatMessage {..} -> chatMessagePublicID)
          chatMessagePublicIDField
      )
    <*> marshallField
      (\ChatMessage {..} -> chatMessageConversationID)
      chatMessageConversationIDField
    <*> marshallField
      (\ChatMessage {..} -> chatMessageOrganizationID)
      chatMessageOrganizationIDField
    <*> marshallField (\ChatMessage {..} -> chatMessageRole) chatMessageRoleField
    <*> marshallField (\ChatMessage {..} -> chatMessageContent) chatMessageContentField
    <*> marshallField
      (\ChatMessage {..} -> chatMessageModelUsed)
      chatMessageModelUsedField
    <*> marshallReadOnly
      ( marshallField
          (\ChatMessage {..} -> chatMessageCreatedAt)
          chatMessageCreatedAtField
      )

-- Message Attachment Marshaller
messageAttachmentMarshaller ::
  SqlMarshaller MessageAttachmentWrite MessageAttachmentRead
messageAttachmentMarshaller =
  MessageAttachment
    <$> marshallReadOnly
      ( marshallField
          (\MessageAttachment {..} -> messageAttachmentID)
          messageAttachmentIDField
      )
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentMessageID)
      messageAttachmentMessageIDField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentOrganizationID)
      messageAttachmentOrganizationIDField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileName)
      messageAttachmentFileNameField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileType)
      messageAttachmentFileTypeField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentFileSizeBytes)
      messageAttachmentFileSizeBytesField
    <*> marshallField
      (\MessageAttachment {..} -> messageAttachmentStoragePath)
      messageAttachmentStoragePathField
    <*> marshallReadOnly
      ( marshallField
          (\MessageAttachment {..} -> messageAttachmentCreatedAt)
          messageAttachmentCreatedAtField
      )

-- Subscription Plan Marshaller
subscriptionPlanMarshaller :: SqlMarshaller SubscriptionPlanWrite SubscriptionPlanRead
subscriptionPlanMarshaller =
  SubscriptionPlan
    <$> marshallReadOnly
      ( marshallField
          (\SubscriptionPlan {..} -> subscriptionPlanID)
          subscriptionPlanIDField
      )
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanName)
      subscriptionPlanNameField
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanPriceCents)
      subscriptionPlanPriceCentsField
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanFeatures)
      subscriptionPlanFeaturesField

-- User Subscription Marshaller
userSubscriptionMarshaller :: SqlMarshaller UserSubscriptionWrite UserSubscriptionRead
userSubscriptionMarshaller =
  UserSubscription
    <$> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionID)
          userSubscriptionIDField
      )
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionOrganizationID)
      userSubscriptionOrganizationIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionPlanID)
      userSubscriptionPlanIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionStripeSubscriptionID)
      userSubscriptionStripeSubscriptionIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionStatus)
      subscriptionStatusField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionCurrentPeriodEndsAt)
      userSubscriptionCurrentPeriodEndsAtField
    <*> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionCreatedAt)
          userSubscriptionCreatedAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionUpdatedAt)
          userSubscriptionUpdatedAtField
      )

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

-- Email Verification OTP Marshaller
emailVerificationOTPMarshaller ::
  SqlMarshaller EmailVerificationOTPWrite EmailVerificationOTPRead
emailVerificationOTPMarshaller =
  EmailVerificationOTP
    <$> marshallReadOnly (marshallField
      (\EmailVerificationOTP {..} -> emailVerificationOTPID)
      emailVerificationOTPIDField)
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

refreshTokenMarshaller :: SqlMarshaller RefreshTokenWrite RefreshTokenRead
refreshTokenMarshaller =
  RefreshToken
    <$> marshallReadOnly
      (marshallField (\RefreshToken {..} -> refreshTokenID) refreshTokenIDField)
    <*> marshallField (\RefreshToken {..} -> refreshTokenUserID) refreshTokenUserIDField
    <*> marshallField (\RefreshToken {..} -> refreshTokenTokenHash) refreshTokenTokenHashField
    <*> marshallField (\RefreshToken {..} -> refreshTokenExpiresAt) refreshTokenExpiresAtField
    <*> marshallReadOnly
      (marshallField (\RefreshToken {..} -> refreshTokenCreatedAt) refreshTokenCreatedAtField)
    <*> marshallField (\RefreshToken {..} -> refreshTokenIsRevoked) refreshTokenIsRevokedField
