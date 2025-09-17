{-# LANGUAGE OverloadedStrings #-}

module TestApp.DB (tests) where

import Modulus.BE.DB
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

clearTestEnvVars :: IO ()
clearTestEnvVars = do
  mapM_
    unsetEnv
    [ "POSTGRES_DB"
    , "POSTGRES_USER"
    , "POSTGRES_PASSWORD"
    , "POSTGRES_HOST"
    ]

-- Helper to set required environment variables
setRequiredEnvVars :: String -> String -> String -> String -> IO ()
setRequiredEnvVars dbName dbUser dbPass dbHost = do
  setEnv "POSTGRES_DB" dbName
  setEnv "POSTGRES_USER" dbUser
  setEnv "POSTGRES_PASSWORD" dbPass
  setEnv "POSTGRES_HOST" dbHost

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
