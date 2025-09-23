{-# LANGUAGE OverloadedStrings #-}

{- |
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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table
  ( apiKeysTable
  , auditLogTable
  , chatMessageTable
  , conversationTable
  , documentEmbeddingTable
  , emailVerificationOTPTable
  , messageAttachmentTable
  , oldConvSummaryTable
  , refreshTokenTable
  , subscriptionPlanTable
  , userSubscriptionTable
  , userTable
  )
import Modulus.BE.DB.Queries.SubscriptionPlan (addSubscriptionPlan)
import Orville.PostgreSQL (ConnectionPool)
import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.AutoMigration
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

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
autoMigrateQ :: (Orville.MonadOrville m) => m ()
autoMigrateQ = do
  let pgcryptoExtensionId = Orville.nameToExtensionId "pgcrypto"
      vectorExtensionId = Orville.nameToExtensionId "vector"
      schemaItems :: [SchemaItem]
      schemaItems =
        [ SchemaExtension pgcryptoExtensionId
        , SchemaExtension vectorExtensionId
        , SchemaTable userTable
        , SchemaTable conversationTable
        , SchemaTable chatMessageTable
        , SchemaTable messageAttachmentTable
        , SchemaTable documentEmbeddingTable
        , SchemaTable subscriptionPlanTable
        , SchemaTable userSubscriptionTable
        , SchemaTable auditLogTable
        , SchemaTable emailVerificationOTPTable
        , SchemaTable refreshTokenTable
        , SchemaTable oldConvSummaryTable
        , SchemaTable apiKeysTable
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
  liftIO $ putStrLn "Inserting subscription plans..."
  subscriptionPlanData

subscriptionPlanData :: Orville.MonadOrville m => m ()
subscriptionPlanData = do
  let freePlan =
        SubscriptionPlan
          { subscriptionPlanID = ()
          , subscriptionPlanName = "Free Plan"
          , subscriptionPlanPriceCents = 0
          , subscriptionPlanFeatures =
              Just "{\"max_messages_per_day\": 50, \"providers\": [\"ollama\"], \"file_uploads\": false}"
          }
  let goldPlan =
        SubscriptionPlan
          { subscriptionPlanID = ()
          , subscriptionPlanName = "Gold Plan"
          , subscriptionPlanPriceCents = 999
          , subscriptionPlanFeatures =
              Just
                "{\"max_messages_per_day\": 500, \"providers\": [\"openrouter\"], \"file_uploads\": true, \"priority_support\": true}"
          }
  let premiumPlan =
        SubscriptionPlan
          { subscriptionPlanID = ()
          , subscriptionPlanName = "Premium Plan"
          , subscriptionPlanPriceCents = 1999
          , subscriptionPlanFeatures =
              Just
                "{\"max_messages_per_day\": -1, \"providers\": [ \"openrouter\", \"ollama\" ], \"file_uploads\": true, \"priority_support\": true, \"custom_models\": true}"
          }
  void $ addSubscriptionPlan freePlan
  void $ addSubscriptionPlan goldPlan
  void $ addSubscriptionPlan premiumPlan
