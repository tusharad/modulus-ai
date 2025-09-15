{- |
Module      : Modulus.BE.DB.Internal.Table
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module defines the Orville 'TableDefinition's for the data types in
the chatbot database schema. A 'TableDefinition' specifies the
structure of a database table, including its name, primary key, the
marshaller used for data conversion, and any associated constraints
(like foreign keys and unique constraints) or indexes.

== Purpose ==

This module acts as the bridge between the abstract Haskell data models
('Modulus.BE.DB.Internal.Model') and the concrete PostgreSQL tables. It tells Orville
how to create the tables in the database and how to map data between the
Haskell types and the table rows. It also codifies the relationships
between tables through foreign key constraints.

== Architectural Role ==

This is a core internal module for the database layer. It is used by
"Modulus.BE.DB.Internal.Schema" to define the overall database schema for migrations
and by query execution logic in other `Modulus.BE.DB.Internal.*` modules (e.g.,
`Modulus.BE.DB.Internal.Queries`) to interact with specific tables.

== Design Notes ==

*   **Orville Integration**: Each 'TableDefinition' is built using Orville
    functions like 'mkTableDefinition', 'primaryKey', 'compositePrimaryKey',
    'addTableConstraints', and 'addTableIndexes'.
*   **Constraints**: Foreign key and unique constraints are defined here
    using 'foreignKeyConstraintWithOptions' and 'uniqueConstraint'. This
    ensures referential integrity is enforced by the database and that
    Orville's schema management is aware of these relationships.
*   **Indexes**: Index definitions (from "DB.Internal.Index") are attached
    to their respective tables using 'addTableIndexes'. This centralizes
    index definitions while associating them with the correct table.
*   **Primary Keys**: The primary key structure (single column or
    composite) is explicitly defined for each table. This is crucial for
    Orville's ability to perform operations like 'findOne' and for
    managing relationships.
*   **Marshaller Link**: Each 'TableDefinition' is linked to its
    corresponding 'SqlMarshaller' from "DB.Internal.Marshaller". This
    marshaller defines the column-level details, including names, types,
    nullability, and defaults.
*   **Table Identity**: The 'tableIdentifier' function can be used to get
    a reference to a table definition, which is necessary for defining
    foreign key relationships between tables within this module.
-}
module Modulus.BE.DB.Internal.Table
  ( -- * User Table
    userTable

    -- * Conversation Table
  , conversationTable

    -- * Chat Message Table
  , chatMessageTable

    -- * Message Attachment Table
  , messageAttachmentTable

    -- * Subscription Plan Table
  , subscriptionPlanTable

    -- * User Subscription Table
  , userSubscriptionTable

    -- * Audit Log Table
  , auditLogTable

    -- * Email Verification OTP Table
  , emailVerificationOTPTable
  , refreshTokenTable
  , documentEmbeddingTable
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.UUID
import Modulus.BE.DB.Internal.Index
import Modulus.BE.DB.Internal.Marshaller
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- USER TABLE
userTable :: TableDefinition (HasKey UserID) UserWrite UserRead
userTable =
  addTableIndexes [idxUsersEmail] $
    addTableConstraints
      [ uniqueConstraint (fieldName userEmailField :| [])
      ]
      ( mkTableDefinition
          "users"
          (primaryKey userIDField)
          userMarshaller
      )

-- CONVERSATION TABLE
conversationTable ::
  TableDefinition (HasKey ConversationID) ConversationWrite ConversationRead
conversationTable =
  addTableIndexes [idxConversationsUserId] $
    addTableConstraints
      [ uniqueConstraint (fieldName conversationPublicIDField :| [])
      , fkConversationToUser
      ]
      ( mkTableDefinition
          "conversations"
          (primaryKey conversationIDField)
          conversationMarshaller
      )
  where
    fkConversationToUser =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName conversationUserIDField)
            (fieldName userIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = SetNull
            , foreignKeyOptionsOnDelete = SetNull
            }
        )

-- CHAT MESSAGE TABLE
chatMessageTable ::
  TableDefinition (HasKey ChatMessageID) ChatMessageWrite ChatMessageRead
chatMessageTable =
  addTableIndexes [idxChatMessagesConversationId] $
    addTableConstraints
      [ uniqueConstraint (fieldName chatMessagePublicIDField :| [])
      , fkChatMessageToConversation
      ]
      ( mkTableDefinition
          "chat_messages"
          (primaryKey chatMessageIDField)
          chatMessageMarshaller
      )
  where
    fkChatMessageToConversation =
      foreignKeyConstraintWithOptions
        (tableIdentifier conversationTable)
        ( foreignReference
            (fieldName chatMessageConversationIDField)
            (fieldName conversationIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )

-- MESSAGE ATTACHMENT TABLE
messageAttachmentTable ::
  TableDefinition
    (HasKey MessageAttachmentID)
    MessageAttachmentWrite
    MessageAttachmentRead
messageAttachmentTable =
  addTableIndexes [idxMessageAttachmentsMessageId] $
    addTableConstraints
      [ fkMessageAttachmentToMessage
      ]
      ( mkTableDefinition
          "message_attachments"
          (primaryKey messageAttachmentIDField)
          messageAttachmentMarshaller
      )
  where
    fkMessageAttachmentToMessage =
      foreignKeyConstraintWithOptions
        (tableIdentifier chatMessageTable)
        ( foreignReference
            (fieldName messageAttachmentMessageIDField)
            (fieldName chatMessageIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )

-- SUBSCRIPTION PLAN TABLE
subscriptionPlanTable ::
  TableDefinition (HasKey SubscriptionPlanID) SubscriptionPlanWrite SubscriptionPlanRead
subscriptionPlanTable =
  mkTableDefinition
    "subscription_plans"
    (primaryKey subscriptionPlanIDField)
    subscriptionPlanMarshaller

-- USER SUBSCRIPTION TABLE
userSubscriptionTable ::
  TableDefinition (HasKey UserSubscriptionID) UserSubscriptionWrite UserSubscriptionRead
userSubscriptionTable =
  addTableConstraints
    [ fkUserSubscriptionToPlan
    ]
    ( mkTableDefinition
        "user_subscriptions"
        (primaryKey userSubscriptionIDField)
        userSubscriptionMarshaller
    )
  where
    fkUserSubscriptionToPlan =
      foreignKeyConstraintWithOptions
        (tableIdentifier subscriptionPlanTable)
        ( foreignReference
            (fieldName userSubscriptionPlanIDField)
            (fieldName subscriptionPlanIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )

-- AUDIT LOG TABLE
auditLogTable :: TableDefinition (HasKey AuditLogID) AuditLogWrite AuditLogRead
auditLogTable =
  addTableIndexes [idxAuditLogUserId, idxAuditLogAction] $
    mkTableDefinition
      "audit_log"
      (primaryKey auditLogIDField)
      auditLogMarshaller

-- EMAIL VERIFICATION OTP TABLE
emailVerificationOTPTable ::
  TableDefinition (HasKey UUID) EmailVerificationOTPWrite EmailVerificationOTPRead
emailVerificationOTPTable =
  addTableConstraints
    [ fkEmailVerificationOTPToUser
    ]
    ( mkTableDefinition
        "email_verification_otps"
        (primaryKey emailVerificationOTPIDField)
        emailVerificationOTPMarshaller
    )
  where
    fkEmailVerificationOTPToUser =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName emailVerificationOTPUserIDField)
            (fieldName userIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )

refreshTokenTable ::
  TableDefinition (HasKey RefreshTokenID) RefreshTokenWrite RefreshTokenRead
refreshTokenTable =
  addTableConstraints
    [ uniqueConstraint (fieldName refreshTokenTokenHashField :| [])
    , fkRefreshTokenToUser
    ]
    ( mkTableDefinition
        "refresh_tokens"
        (primaryKey refreshTokenIDField)
        refreshTokenMarshaller
    )
  where
    fkRefreshTokenToUser =
      foreignKeyConstraintWithOptions
        (tableIdentifier userTable)
        ( foreignReference
            (fieldName refreshTokenUserIDField)
            (fieldName userIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )

documentEmbeddingTable ::
  TableDefinition
    (HasKey DocumentEmbeddingID)
    DocumentEmbeddingWrite
    DocumentEmbeddingRead
documentEmbeddingTable =
  addTableConstraints
    [fkDocumentEmbeddingToMessageAttachment]
    ( mkTableDefinition
        "document_embedding"
        (primaryKey documentEmbeddingIDField)
        documentEmbeddingMarshaller
    )
  where
    fkDocumentEmbeddingToMessageAttachment =
      foreignKeyConstraintWithOptions
        (tableIdentifier messageAttachmentTable)
        ( foreignReference
            (fieldName documentEmbeddingMessageAttachmentIDField)
            (fieldName messageAttachmentIDField)
            :| []
        )
        ( defaultForeignKeyOptions
            { foreignKeyOptionsOnUpdate = Cascade
            , foreignKeyOptionsOnDelete = Cascade
            }
        )
