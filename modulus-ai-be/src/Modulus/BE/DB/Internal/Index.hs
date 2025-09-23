{- |
Module      : Modulus.BE.DB.Internal.Index
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module defines the Orville 'IndexDefinition's for the database tables
in the chatbot schema. Indexes are crucial for optimizing query
performance by allowing the database to quickly locate rows without
performing full table scans.

== Purpose ==

Each 'IndexDefinition' in this module corresponds to a @CREATE INDEX@
statement in the SQL schema. These definitions specify which columns
(or expressions) should be indexed and whether the index should enforce
uniqueness. Orville uses these definitions during auto-migrations to
ensure the necessary indexes exist in the database.

== Architectural Role ==

This is an internal module that encapsulates all index definitions.
It is consumed by "Modulus.BE.DB.Internal.Table", where the indexes are associated
with their respective 'TableDefinition's using 'Orville.addTableIndexes'.
This centralizes index management while keeping the definitions tied to
the correct tables.

== Design Notes ==

*   **Orville Integration**: Indexes are defined using functions from
    'Orville.PostgreSQL.Schema.IndexDefinition' such as 'nonUniqueIndex'
    and 'uniqueIndex'. These functions create 'IndexDefinition' values
    that Orville can use for schema management.
*   **Index Keys**: Each 'IndexDefinition' has an internal
    'IndexMigrationKey'. Orville uses this key to determine if an index
    already exists in the database. The key is typically based on the
    index's name or its structural attributes (columns, uniqueness).
*   **Association with Tables**: These 'IndexDefinition's are not
    automatically linked to tables here. The association happens in
    "DB.Internal.Table" via 'Orville.addTableIndexes'. This allows
    reusing index definitions if needed (though uncommon) and keeps
    table definitions as the central point for table structure.
*   **Performance Considerations**: Indexes speed up @SELECT@ queries
    but can slow down @INSERT@, @UPDATE@, and @DELETE@ operations as
    the indexes need to be maintained. Indexes should be created
    judiciously based on actual query patterns.
*   **Migration Strategy**: Orville supports different index creation
    strategies (e.g., 'Transactional', 'Concurrent'). The default is
    'Transactional', which is used here. 'Concurrent' creation is
    useful for large tables in production but requires special handling
    as it runs outside the main migration transaction.
-}
module Modulus.BE.DB.Internal.Index
  ( -- * Users Indexes
    idxUsersEmail
  , idxConversationsUserId

    -- * Chat Messages Indexes
  , idxChatMessagesConversationId

    -- * Message Attachments Indexes
  , idxMessageAttachmentsMessageId

    -- * Audit Log Indexes
  , idxAuditLogUserId
  , idxAuditLogAction
  , idxOldConvSummaryConversationId

    -- * API Keys Indexes
  , idxApiKeysUserIdAndProviderName
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Modulus.BE.DB.Internal.Marshaller
import Orville.PostgreSQL
import qualified Orville.PostgreSQL.Schema.IndexDefinition as IndexDefinition

-----------------------------------------------------------------------------
-- Users Indexes
-----------------------------------------------------------------------------

-- CREATE INDEX idx_users_email ON users(email);
idxUsersEmail :: IndexDefinition
idxUsersEmail =
  IndexDefinition.nonUniqueIndex (fieldName userEmailField :| [])

-----------------------------------------------------------------------------
-- Conversations Indexes
-----------------------------------------------------------------------------
-- CREATE INDEX idx_conversations_user_id ON conversations(user_id);
idxConversationsUserId :: IndexDefinition
idxConversationsUserId =
  IndexDefinition.nonUniqueIndex (fieldName conversationUserIDField :| [])

-----------------------------------------------------------------------------
-- Chat Messages Indexes
-----------------------------------------------------------------------------

-- CREATE INDEX idx_chat_messages_conversation_id ON chat_messages(conversation_id);
idxChatMessagesConversationId :: IndexDefinition
idxChatMessagesConversationId =
  IndexDefinition.nonUniqueIndex (fieldName chatMessageConversationIDField :| [])

-----------------------------------------------------------------------------
-- Message Attachments Indexes
-----------------------------------------------------------------------------

-- CREATE INDEX idx_message_attachments_message_id ON message_attachments(message_id);
idxMessageAttachmentsMessageId :: IndexDefinition
idxMessageAttachmentsMessageId =
  IndexDefinition.nonUniqueIndex (fieldName messageAttachmentMessageIDField :| [])

-----------------------------------------------------------------------------
-- Audit Log Indexes
-----------------------------------------------------------------------------

-- CREATE INDEX idx_audit_log_user_id ON audit_log(user_id);
idxAuditLogUserId :: IndexDefinition
idxAuditLogUserId =
  IndexDefinition.nonUniqueIndex (fieldName auditLogUserIDField :| [])

-- CREATE INDEX idx_audit_log_action ON audit_log(action);
idxAuditLogAction :: IndexDefinition
idxAuditLogAction =
  IndexDefinition.nonUniqueIndex (fieldName auditLogActionField :| [])

idxOldConvSummaryConversationId :: IndexDefinition
idxOldConvSummaryConversationId =
  IndexDefinition.nonUniqueIndex (fieldName oldConvSummaryConversationIDField :| [])

idxApiKeysUserIdAndProviderName :: IndexDefinition
idxApiKeysUserIdAndProviderName =
  IndexDefinition.uniqueIndex (fieldName apiKeyUserIDField :| [fieldName apiKeyProviderNameField])
