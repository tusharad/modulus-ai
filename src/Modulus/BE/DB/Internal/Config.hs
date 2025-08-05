{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modulus.BE.DB.Internal.Config
  ( ConnectionConfigError (..)
  , mkConnectionPoolFromEnv
  ) where

import Control.Exception
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Orville.PostgreSQL
import qualified Orville.PostgreSQL as Orville
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Errors that can occur while reading configuration from environment variables.
data ConnectionConfigError
  = -- | One or more required environment variables were not set.
    MissingRequiredEnvVars (NonEmpty String)
  | -- | An environment variable had a value that could not be parsed (VarName, Value).
    InvalidEnvVarFormat String String
  | -- | An unexpected IO error occurred while reading an environment variable.
    EnvVarIOError String IOException
  | -- | Unexpected error
    Unexpected String
  deriving (Show)

-- Make it an exception so it can be thrown/caught if needed in IO contexts
instance Exception ConnectionConfigError

-- | The names of the environment variables used for configuration.
dbNameEnvVar :: String
dbNameEnvVar = "MODULUS_DB_NAME"

dbUserEnvVar :: String
dbUserEnvVar = "MODULUS_DB_USER_NAME"

dbPasswordEnvVar :: String
dbPasswordEnvVar = "MODULUS_DB_PASSWORD"

dbHostEnvVar :: String
dbHostEnvVar = "MODULUS_DB_HOST"

timeoutEnvVar :: String
timeoutEnvVar = "MODULUS_DB_TIMEOUT" -- Interpreted as seconds for connection timeout

{- | Attempts to read a single environment variable.
Returns 'Right (Just value)' if found, 'Right Nothing' if not found, or 'Left' on IO error.
-}
lookupEnvSafe :: String -> IO (Either ConnectionConfigError (Maybe String))
lookupEnvSafe varName =
  (Right <$> lookupEnv varName) `catch` \(e :: IOException) ->
    return $ Left $ EnvVarIOError varName e

-- | Reads an environment variable, returning an error if it's missing.
readRequiredEnvVar :: String -> IO (Either ConnectionConfigError String)
readRequiredEnvVar varName = do
  result <- lookupEnvSafe varName
  case result of
    Left err -> return $ Left err
    Right Nothing -> return $ Left $ MissingRequiredEnvVars (varName :| [])
    Right (Just val) -> return $ Right val

-- | Reads an optional environment variable and attempts to parse it.
readOptionalEnvVar :: Read a => String -> IO (Either ConnectionConfigError (Maybe a))
readOptionalEnvVar varName = do
  result <- lookupEnvSafe varName
  case result of
    Left err -> return $ Left err
    Right Nothing -> return $ Right Nothing
    Right (Just valStr) -> case readMaybe valStr of
      Nothing -> return $ Left $ InvalidEnvVarFormat varName valStr
      Just val -> return $ Right (Just val)

{- | Builds a basic PostgreSQL keyword/value connection string.
Format: key1=value1 key2=value2 ...
Values are escaped using escapeConnStrValue.
-}
buildConnectionString :: [(String, String)] -> String
buildConnectionString = unwords . map (\(k, v) -> k ++ "=" ++ v)

{- |
  Creates 'Orville.ConnectionPool' by reading configuration from
  environment variables.

  This function reads the following environment variables:

  * __Required:__
      * @MODULUS_DB_NAME@: The name of the PostgreSQL database.
      * @MODULUS_DB_USER_NAME@: The username to connect to the database.
      * @MODULUS_DB_PASSWORD@: The password for the database user.
      * @MODULUS_DB_HOST@: The hostname or IP address of the database server.

  * __Optional:__
      * @MODULUS_DB_TIMEOUT@: Connection timeout in seconds (integer).
        If not provided, the underlying library's default is used.

  If any required environment variable is missing, or if an optional variable
  is present but cannot be parsed, this function returns a 'Left' with a
  descriptive 'ConnectionConfigError'.

  This function constructs a PostgreSQL connection string manually,
  handling basic escaping for values. It then configures the Orville options
  based on the parsed environment variables.
-}
mkConnectionPoolFromEnv :: IO (Either ConnectionConfigError Orville.ConnectionPool)
mkConnectionPoolFromEnv = do
  dbNameResult <- readRequiredEnvVar dbNameEnvVar
  dbUserResult <- readRequiredEnvVar dbUserEnvVar
  dbPasswordResult <- readRequiredEnvVar dbPasswordEnvVar
  dbHostResult <- readRequiredEnvVar dbHostEnvVar
  timeoutResult <- readOptionalEnvVar timeoutEnvVar

  let requiredResults = [dbNameResult, dbUserResult, dbPasswordResult, dbHostResult]
  let (requiredErrors, requiredValues) = partitionEithers requiredResults

  let optionalResults = [timeoutResult]
  let (optionalErrors, optionalValues) = partitionEithers optionalResults

  let allErrors = requiredErrors <> optionalErrors

  case allErrors of
    [] -> do
      case requiredValues of
        [dbName_, dbUser, dbPassword_, dbHost] -> do
          case optionalValues of
            [mbTimeout] -> do
              let connStrKVs =
                    catMaybes
                      [ Just ("host", dbHost)
                      , Just ("dbname", dbName_)
                      , Just ("user", dbUser)
                      , Just ("password", dbPassword_)
                      , fmap (\t -> ("connect_timeout", show (t :: Int))) mbTimeout
                      ]
              let cString = buildConnectionString connStrKVs
              let baseOptions =
                    ConnectionOptions
                      { Orville.connectionString = cString
                      , connectionNoticeReporting = DisableNoticeReporting
                      , connectionPoolStripes = OneStripePerCapability
                      , connectionPoolMaxConnections = MaxConnectionsPerStripe 1
                      , connectionPoolLingerTime = 10
                      }
              eRes <- try $ createConnectionPool baseOptions
              case eRes of
                Left excp -> pure $ Left $ Unexpected (show (excp :: SomeException))
                Right pool -> pure $ Right pool
            _ -> pure $ Left $ Unexpected "Some values are missing"
        _ -> pure $ Left $ Unexpected "Some values are missing"
    (firstErr : _) -> do
      let missingRequired =
            [var | Left (MissingRequiredEnvVars (var :| _)) <- requiredResults]
      case NE.nonEmpty missingRequired of
        Just missingVarsNE -> return $ Left $ MissingRequiredEnvVars missingVarsNE
        Nothing -> return $ Left firstErr
