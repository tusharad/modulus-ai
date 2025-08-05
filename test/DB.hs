{-# LANGUAGE OverloadedStrings #-}

module DB (tests) where

import Modulus.BE.DB
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

clearTestEnvVars :: IO ()
clearTestEnvVars = do
  mapM_
    unsetEnv
    [ "MODULUS_DB_NAME"
    , "MODULUS_DB_USER_NAME"
    , "MODULUS_DB_PASSWORD"
    , "MODULUS_DB_HOST"
    ]

-- Helper to set required environment variables
setRequiredEnvVars :: String -> String -> String -> String -> IO ()
setRequiredEnvVars dbName dbUser dbPass dbHost = do
  setEnv "MODULUS_DB_NAME" dbName
  setEnv "MODULUS_DB_USER_NAME" dbUser
  setEnv "MODULUS_DB_PASSWORD" dbPass
  setEnv "MODULUS_DB_HOST" dbHost

testCreateConnectionPool :: TestTree
testCreateConnectionPool = testCase "Successful Configuration Parsing" $ do
  clearTestEnvVars
  setRequiredEnvVars "testdb" "testuser" "testpass" "localhost"
  result <- mkConnectionPoolFromEnv
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Modulus.BE.DB Tests"
    [testCreateConnectionPool]
