{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Modulus.BE.DB.Internal.Schema
Copyright   : (c) 2025 Tushar
License     : All Rights Reserved
Maintainer  : tusharadhatrao@gmail.com
Stability   : Experimental
Portability : Unix

This module defines `autoMigrateQ` function.
This function shall be called at the start of the application
to make sure database schema is up to date.
-}
module Modulus.BE.DB.Internal.Schema
  ( autoMigrate
   , autoMigrateQ
  ) where

import Modulus.BE.DB.Internal.Table
  ( auditLogTable
  , chatMessageTable
  , conversationTable
  , emailVerificationOTPTable
  , messageAttachmentTable
  , organizationMemberTable
  , organizationTable
  , subscriptionPlanTable
  , userSubscriptionTable
  , userTable
  , refreshTokenTable
  )
import Control.Monad.IO.Class (liftIO)
import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.AutoMigration
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import Orville.PostgreSQL (ConnectionPool)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Data.ByteString.Char8 as BS

autoMigrate :: ConnectionPool -> IO ()
autoMigrate pool = Orville.runOrville pool autoMigrateQ

{- |
  Automatically migrates the database schema to match the current Haskell
  table definitions.

  This function will:
  1. Print a summary of each schema item (table) being considered for migration.
  2. Use Orville's 'autoMigrateSchema' to compare the current database schema
     against the definitions provided and apply any necessary changes (like
  '   creating tables, adding columns, adding constraints/indexes).

  This is typically called during application startup to ensure the database
  is up-to-date.
-}
autoMigrateQ :: Orville.MonadOrville m => m ()
autoMigrateQ = do
  let
      pgcryptoExtensionId = Orville.nameToExtensionId "pgcrypto"
      schemaItems :: [SchemaItem]
      schemaItems =
        [ SchemaExtension pgcryptoExtensionId
        , SchemaTable organizationTable
        , SchemaTable userTable
        , SchemaTable organizationMemberTable
        , SchemaTable conversationTable
        , SchemaTable chatMessageTable
        , SchemaTable messageAttachmentTable
        , SchemaTable subscriptionPlanTable
        , SchemaTable userSubscriptionTable
        , SchemaTable auditLogTable
        , SchemaTable emailVerificationOTPTable
        , SchemaTable refreshTokenTable
        ]

  -- TODO: Add test case to make sure pgcrypto is being added
  -- Print summaries of the schema items
  liftIO $ do
    putStrLn "=== Database Schema Migration Summary ==="
    mapM_ (putStrLn . ("- " ++) . schemaItemSummary) schemaItems
    putStrLn "========================================"
    putStrLn "Starting auto-migration..."


  plan <- AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItems
  let steps = AutoMigration.migrationPlanSteps plan
  liftIO $ do
    putStrLn "Migration plan (Queries that are going to be executed):"
    mapM_ (BS.putStrLn . RawSql.toExampleBytes) steps

  AutoMigration.autoMigrateSchema AutoMigration.defaultOptions schemaItems
  liftIO $ putStrLn "Database schema migration completed."
