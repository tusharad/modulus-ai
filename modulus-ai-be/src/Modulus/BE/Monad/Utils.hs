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
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Modulus.BE.DB.Internal.Config (mkConnectionPoolFromEnv)
import Modulus.BE.Monad.AppM
import Modulus.BE.Monad.Error (AppError)
import Modulus.Common.Openrouter
import Modulus.Common.Types
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Ollama
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
  ePort <- lookupEnv "MODULUS_APP_PORT"
  eJwtSecret <- lookupEnv "MODULUS_JWT_SECRET"
  eLogLevel <- lookupEnv "MODULUS_LOG_LEVEL"
  eEnvironment <- lookupEnv "MODULUS_ENVIRONMENT"
  eRedisUrl <- lookupEnv "MODULUS_REDIS_URL"
  eMailgunApi <- lookupEnv "MODULUS_MAILGUN_API"
  apiTimeout <- readEnvWithDefault "MODULUS_API_TIMEOUT" 30
  loggerSet <- newStdoutLoggerSet defaultBufSize
  eConnectionPool <- mkConnectionPoolFromEnv
  case eConnectionPool of
    Left err -> pure $ Left (T.pack $ show err)
    Right connPool -> do
      case (ePort, eJwtSecret, eMailgunApi) of
        (Just portStr, Just jwtSecret, Just mailGunApi) -> do
          case reads portStr of
            [(port, "")] -> do
              manager <- HTTP.newTlsManager
              ollamaModelList <- listAvailableOllamaModels
              openRouterModels <-
                map modelId . take 5 . fromRight []
                  <$> getOpenRouterModelList
              let modelProviders =
                    [ ModelProviders
                        { providerName = "ollama"
                        , modelList = ollamaModelList
                        , isApiFieldRequired = False
                        }
                    , ModelProviders
                        { providerName = "openrouter"
                        , modelList = openRouterModels
                        , isApiFieldRequired = True
                        }
                    ]
              let orvilleState =
                    O.newOrvilleState O.defaultErrorDetailLevel connPool
              pure $
                Right
                  AppConfig
                    { configHttpManager = manager
                    , configPort = port
                    , configLogLevel = maybe "INFO" T.pack eLogLevel
                    , configEnvironment = maybe "development" T.pack eEnvironment
                    , configRedisUrl = T.pack <$> eRedisUrl
                    , configJwtSecret = T.pack jwtSecret
                    , configExternalApiTimeout = apiTimeout
                    , configLoggerSet = loggerSet
                    , configMinLogLevel = Debug
                    , configOrvilleState = orvilleState
                    , configMailGunApiKey = T.pack mailGunApi
                    , configCurrentProviders = modelProviders
                    }
            _ -> pure $ Left "Invalid PORT environment variable"
        _ ->
          pure $ Left "Missing required environment variables: PORT, JWT_SECRET"
  where
    readEnvWithDefault :: Read a => String -> a -> IO a
    readEnvWithDefault envVar defaultVal = do
      r <- lookupEnv envVar
      case r of
        Nothing -> pure defaultVal
        Just str -> case reads str of
          [(val, "")] -> pure val
          _ -> pure defaultVal

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

listAvailableOllamaModels :: IO [Text]
listAvailableOllamaModels = do
  eVersion <- Ollama.getVersion
  case eVersion of
    Left _ -> do
      putStrLn "Ollama not seems to be installed."
      pure []
    Right (Ollama.Version v) -> do
      putStrLn $ "Ollama version: " <> T.unpack v
      eModelInfo <- Ollama.list Nothing
      case eModelInfo of
        Left err -> print err >> pure []
        Right (Ollama.Models modelInfoList) -> do
          let modelNames = map Ollama.name modelInfoList
          pure modelNames
