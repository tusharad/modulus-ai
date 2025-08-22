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
module Modulus.BE.DB.Internal.Marshaller
  ( module Export
  ) where

import Modulus.BE.DB.Internal.Marshaller.AuditLog as Export
import Modulus.BE.DB.Internal.Marshaller.ChatMessage as Export
import Modulus.BE.DB.Internal.Marshaller.Conversation as Export
import Modulus.BE.DB.Internal.Marshaller.EmailVerificationOTP as Export
import Modulus.BE.DB.Internal.Marshaller.MessageAttachment as Export
import Modulus.BE.DB.Internal.Marshaller.RefreshToken as Export
import Modulus.BE.DB.Internal.Marshaller.SubscriptionPlan as Export
import Modulus.BE.DB.Internal.Marshaller.User as Export
import Modulus.BE.DB.Internal.Marshaller.UserSubscription as Export
