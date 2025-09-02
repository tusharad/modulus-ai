{-# LANGUAGE OverloadedStrings #-}

module Modulus.BE.Monad.Utils
  ( mkAppConfigFromEnv
  , askConfig
  , throwAppError
  , catchAppError
  , handleAppError
  , withResource
  , hmacJwk
  ) where

import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Crypto.JOSE.Types
import Crypto.JWT
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Modulus.BE.DB.Internal.Config (mkConnectionPoolFromEnv)
import Modulus.BE.LLM.Providers (fetchAllProviders)
import Modulus.BE.Monad.AppM
import Modulus.BE.Monad.Error (AppError)
import Modulus.Common.Types
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Orville.PostgreSQL as O
import System.Environment (lookupEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import UnliftIO (UnliftIO (unliftIO), askUnliftIO, bracket)

hmacJwk :: T.Text -> JWK
hmacJwk secret =
  fromKeyMaterial
    ( OctKeyMaterial . OctKeyParameters . Base64Octets $
        TE.encodeUtf8 secret
    )

-- | Create AppConfig from environment variables
mkAppConfigFromEnv :: IO (Either Text AppConfig)
mkAppConfigFromEnv = do
  mbPort <- lookupEnv "MODULUS_APP_PORT"
  mbJwtSecret <- lookupEnv "MODULUS_JWT_SECRET"
  mbLogLevel <- lookupEnv "MODULUS_LOG_LEVEL"
  mbEnvironment <- lookupEnv "MODULUS_ENVIRONMENT"
  mbRedisUrl <- lookupEnv "MODULUS_REDIS_URL"
  mbMailgunApi <- lookupEnv "MODULUS_MAILGUN_API"
  mbUploadFilePath <- lookupEnv "MODULUS_FILE_UPLOAD_PATH"
  apiTimeout <- readEnvWithDefault "MODULUS_API_TIMEOUT" 30
  loggerSet <- newStdoutLoggerSet defaultBufSize
  let env = fromMaybe Local (textToEnv (fromMaybe "" mbEnvironment))
  print ("ENVIRONMENT SET for " :: String, env)
  eConnectionPool <- mkConnectionPoolFromEnv
  case eConnectionPool of
    Left err -> pure $ Left (T.pack $ show err)
    Right connPool -> do
      case (mbPort, mbJwtSecret, mbMailgunApi, mbUploadFilePath) of
        ( Just portStr
          , Just jwtSecret
          , Just mailGunApi
          , Just fileUploadPath
          ) -> do
            case reads portStr of
              [(port, "")] -> do
                manager <- HTTP.newTlsManager
                modelProviders <- fetchAllProviders
                let orvilleState =
                      O.newOrvilleState O.defaultErrorDetailLevel connPool
                pure $
                  Right
                    AppConfig
                      { configHttpManager = manager
                      , configPort = port
                      , configEnvironment = env
                      , configRedisUrl = T.pack <$> mbRedisUrl
                      , configJwtSecret = T.pack jwtSecret
                      , configExternalApiTimeout = apiTimeout
                      , configLoggerSet = loggerSet
                      , configMinLogLevel =
                          fromMaybe Debug (parseLogLevel =<< mbLogLevel)
                      , configOrvilleState = orvilleState
                      , configMailGunApiKey = T.pack mailGunApi
                      , configCurrentProviders = modelProviders
                      , configFileUploadPath = fileUploadPath
                      }
              _ -> pure $ Left "Invalid PORT environment variable "
        (mb1, mb2, mb3, mb4) ->
          pure $
            Left $
              "Missing required environment variables: "
                <> mconcat
                  [ maybe "PORT" (const "") mb1
                  , maybe "JWT Secret" (const "") mb2
                  , maybe "Mail gun api" (const "") mb3
                  , maybe "File upload path" (const "") mb4
                  ]
  where
    readEnvWithDefault :: (Read a) => String -> a -> IO a
    readEnvWithDefault envVar defaultVal = do
      r <- lookupEnv envVar
      case r of
        Nothing -> pure defaultVal
        Just str -> case reads str of
          [(val, "")] -> pure val
          _ -> pure defaultVal

textToEnv :: String -> Maybe Environment
textToEnv "Local" = Just Local
textToEnv "Development" = Just Development
textToEnv "Production" = Just Production
textToEnv _ = Nothing

-- | Utility functions
askConfig :: AppM AppConfig
askConfig = asks id

-- | Error handling utilities
throwAppError :: AppError -> AppM a
throwAppError = throwError

catchAppError :: AppM a -> (AppError -> AppM a) -> AppM a
catchAppError = catchError

handleAppError :: AppM a -> (AppError -> AppM a) -> AppM a
handleAppError = catchAppError

-- | Safe resource management with proper cleanup
withResource :: IO a -> (a -> IO b) -> (a -> AppM c) -> AppM c
withResource acquire release action = do
  unlift <- askUnliftIO
  liftIO $ bracket acquire release (unliftIO unlift . action)
