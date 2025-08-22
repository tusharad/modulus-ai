{- |
Module      : Modulus.BE.DB
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module serves as the primary and **exclusive** public interface for all
database operations within the Modulus backend. It follows the Facade design
pattern, providing a stable, high-level API for the rest of the application
to interact with the data persistence layer.

== Architectural Contract ==

All other backend modules (e.g., API handlers, business logic services) **MUST**
only import this `Modulus.BE.DB` module for their database needs. They **MUST NOT**
import any of the internal `Modulus.BE.DB.Internal.*` modules directly. This contract
is crucial for maintaining a clean architecture, ensuring that the internal
database implementation (table structures, query logic, etc.) can be
refactored without breaking consumer code.

This module aggregates and re-exports curated functions and types from
various internal modules, which handle specific database tables or concerns, such as:
  - User authentication and profile management (`Modulus.BE.DB.Internal.User`)
  - Conversation and message handling (`Modulus.BE.DB.Internal.Conversation`)
  - Organization and subscription logic (`Modulus.BE.DB.Internal.Organization`)

By centralizing the database API here, we ensure consistency, enforce
multi-tenancy rules at a single point, and provide a clear boundary for all
data access logic.
-}
module Modulus.BE.DB
  ( -- * Schema
    autoMigrate

    -- * Connection
  , mkConnectionPoolFromEnv
  ) where

import Modulus.BE.DB.Internal.Config (mkConnectionPoolFromEnv)
import Modulus.BE.DB.Internal.Schema (autoMigrate)
