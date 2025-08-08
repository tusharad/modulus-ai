{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Modulus.BE.DB.Internal.Model
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module defines the core Haskell data types that represent the entities
within the chatbot database schema. It serves as the foundational
data layer for the application's database interactions.

== Purpose ==

The types defined here directly correspond to the tables and enums in the
PostgreSQL database. They are used by the Orville ORM for marshalling data
between the Haskell application and the database rows. These types are
parameterized to distinguish between write contexts (e.g., inserting a new
record) and read contexts (e.g., reading a record with its generated ID and
timestamps).

== Architectural Role ==

This module is part of the internal database implementation. It is intended
to be imported and used by other `DB.Internal.*` modules (e.g.,
`Modulus.BE.DB.Internal.Table`, `Modulus.BE.DB.Internal.Marshaller`, `Modulus.BE.DB.Internal.Queries`) which
build upon these types to provide the database access logic.

== Design Notes ==

*   **Phantom Types**: Many data types use phantom types (e.g., `a`, `b`) to
    differentiate between write (@Write@) and read (@Read@) variants. This
    provides compile-time guarantees about the presence of fields like

    newtype RefreshTokenID = RefreshTokenID UUID
      deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

      data RefreshToken a b = RefreshToken
        { refreshTokenID :: a
          , refreshTokenUserID :: UserID
            , refreshTokenTokenHash :: Text
              , refreshTokenExpiresAt :: UTCTime
                , refreshTokenCreatedAt :: b
                  , refreshTokenIsRevoked :: Bool
                    }
                      deriving (Show, Eq, Generic, ToJSON)

                      type RefreshTokenRead = RefreshToken RefreshTokenID UTCTime
                      type RefreshTokenWrite = RefreshToken () ()
    auto-generated IDs and timestamps.
*   **Separation of Concerns**: This module focuses solely on data
    representation. Database operations, schema definitions, and marshalling
    logic are handled by other dedicated internal modules.
*   **Orville Integration**: The structure of these types is designed to work
    seamlessly with the Orville ORM's marshalling and table definition
    mechanisms.
-}
module Modulus.BE.DB.Internal.Model
  ( -- * User Types
    UserID (..)
  , User (..)
  , UserRead
  , UserWrite

    -- * Enum Types
  , UserRole (..)
  , MessageRole (..)
  , SubscriptionStatus (..)

    -- * Conversation Types
  , ConversationID (..)
  , ConversationPublicID (..)
  , Conversation (..)
  , ConversationRead
  , ConversationWrite

    -- * Chat Message Types
  , ChatMessageID (..)
  , ChatMessagePublicID (..)
  , ChatMessage (..)
  , ChatMessageRead
  , ChatMessageWrite

    -- * Message Attachment Types
  , MessageAttachmentID (..)
  , MessageAttachment (..)
  , MessageAttachmentRead
  , MessageAttachmentWrite

    -- * Subscription Plan Types
  , SubscriptionPlanID (..)
  , SubscriptionPlan (..)
  , SubscriptionPlanRead
  , SubscriptionPlanWrite

    -- * User Subscription Types
  , UserSubscriptionID (..)
  , UserSubscription (..)
  , UserSubscriptionRead
  , UserSubscriptionWrite

    -- * Audit Log Types
  , AuditLogID (..)
  , AuditLog (..)
  , AuditLogRead
  , AuditLogWrite

    -- * Email Verification Types
  , EmailVerificationOTP (..)
  , EmailVerificationOTPRead
  , EmailVerificationOTPWrite

    -- * Refresh token types
  , RefreshToken (..)
  , RefreshTokenID (..)
  , RefreshTokenRead
  , RefreshTokenWrite
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics
import GHC.Int (Int32, Int64)

-- Organization Model
newtype OrganizationID = OrganizationID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data Organization a b = Organization
  { organizationID :: a
  , organizationName :: Text
  , organizationCreatedAt :: b
  , organizationUpdatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

-- User Model
newtype UserID = UserID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data User a b = User
  { userID :: a
  , userEmail :: Text
  , userHashedPassword :: Text
  , userLastLoginAt :: Maybe UTCTime
  , userCreatedAt :: b
  , userUpdatedAt :: b
  , userIsEmailVerified :: Bool
  , userFailedLoginAttempts :: Int32
  , userLockedUntil :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserRead = User UserID UTCTime
type UserWrite = User () ()

-- User Role Enum
data UserRole = UserRoleAdmin | UserRoleMember
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Organization Member Model
data OrganizationMember a = OrganizationMember
  { organizationMemberOrganizationID :: OrganizationID
  , organizationMemberUserID :: UserID
  , organizationMemberRole :: UserRole
  , organizationMemberCreatedAt :: a
  }
  deriving (Show, Eq, Generic, ToJSON)

-- Conversation Model
newtype ConversationID = ConversationID Int64
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

newtype ConversationPublicID = ConversationPublicID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data Conversation a b c = Conversation
  { conversationID :: a
  , conversationPublicID :: b
  , conversationUserID :: Maybe UserID
  , conversationTitle :: Text
  , conversationCreatedAt :: c
  , conversationUpdatedAt :: c
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type ConversationRead = Conversation ConversationID ConversationPublicID UTCTime
type ConversationWrite = Conversation () () ()

-- Message Role Enum
data MessageRole
  = MessageRoleUser
  | MessageRoleAssistant
  | MessageRoleSystem
  | MessageRoleTool
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Chat Message Model
newtype ChatMessageID = ChatMessageID Int64
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

newtype ChatMessagePublicID = ChatMessagePublicID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data ChatMessage a b c = ChatMessage
  { chatMessageID :: a
  , chatMessagePublicID :: b
  , chatMessageConversationID :: ConversationID
  , chatMessageRole :: MessageRole
  , chatMessageContent :: Text
  , chatMessageModelUsed :: Maybe Text
  , chatMessageCreatedAt :: c
  }
  deriving (Show, Eq, Generic, ToJSON)

type ChatMessageRead = ChatMessage ChatMessageID ChatMessagePublicID UTCTime
type ChatMessageWrite = ChatMessage () () ()

-- Message Attachment Model
newtype MessageAttachmentID = MessageAttachmentID Int64
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data MessageAttachment a b = MessageAttachment
  { messageAttachmentID :: a
  , messageAttachmentMessageID :: ChatMessageID
  , messageAttachmentFileName :: Text
  , messageAttachmentFileType :: Text
  , messageAttachmentFileSizeBytes :: Int64
  , messageAttachmentStoragePath :: Text
  , messageAttachmentCreatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type MessageAttachmentRead = MessageAttachment MessageAttachmentID UTCTime
type MessageAttachmentWrite = MessageAttachment () ()

-- Subscription Plan Model
newtype SubscriptionPlanID = SubscriptionPlanID Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data SubscriptionPlan a = SubscriptionPlan
  { subscriptionPlanID :: a
  , subscriptionPlanName :: Text
  , subscriptionPlanPriceCents :: Int32
  , subscriptionPlanFeatures :: Maybe Text -- JSONB field
  }
  deriving (Show, Eq, Generic, ToJSON)

type SubscriptionPlanRead = SubscriptionPlan SubscriptionPlanID
type SubscriptionPlanWrite = SubscriptionPlan ()

-- Subscription Status Enum
data SubscriptionStatus
  = SubscriptionStatusActive
  | SubscriptionStatusPastDue
  | SubscriptionStatusCanceled
  | SubscriptionStatusTrialing
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- User Subscription Model
newtype UserSubscriptionID = UserSubscriptionID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data UserSubscription a b = UserSubscription
  { userSubscriptionID :: a
  , userSubscriptionPlanID :: SubscriptionPlanID
  , userSubscriptionStripeSubscriptionID :: Maybe Text
  , userSubscriptionStatus :: SubscriptionStatus
  , userSubscriptionCurrentPeriodEndsAt :: Maybe UTCTime
  , userSubscriptionCreatedAt :: b
  , userSubscriptionUpdatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type UserSubscriptionRead = UserSubscription UserSubscriptionID UTCTime
type UserSubscriptionWrite = UserSubscription () ()

-- Audit Log Model
newtype AuditLogID = AuditLogID Int64
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data AuditLog a b = AuditLog
  { auditLogID :: a
  , auditLogUserID :: Maybe UserID
  , auditLogAction :: Text
  , auditLogDetails :: Maybe Text -- JSONB field
  , auditLogIPAddress :: Maybe Text -- Representing INET as Text
  , auditLogCreatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type AuditLogRead = AuditLog AuditLogID UTCTime
type AuditLogWrite = AuditLog () ()

-- Email Verification OTP Model
data EmailVerificationOTP a b = EmailVerificationOTP
  { emailVerificationOTPID :: a
  , emailVerificationOTPUserID :: UserID
  , emailVerificationOTPOtpHash :: Text
  , emailVerificationOTPExpiresAt :: UTCTime
  , emailVerificationOTPCreatedAt :: b
  }
  deriving (Show, Eq, Generic, ToJSON)

type EmailVerificationOTPRead = EmailVerificationOTP UUID UTCTime
type EmailVerificationOTPWrite = EmailVerificationOTP () ()

newtype RefreshTokenID = RefreshTokenID UUID
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data RefreshToken a b = RefreshToken
  { refreshTokenID :: a
  , refreshTokenUserID :: UserID
  , refreshTokenTokenHash :: Text
  , refreshTokenExpiresAt :: UTCTime
  , refreshTokenCreatedAt :: b
  , refreshTokenIsRevoked :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

type RefreshTokenRead = RefreshToken RefreshTokenID UTCTime
type RefreshTokenWrite = RefreshToken () ()
