{-# LANGUAGE OverloadedStrings #-}

module Modulus.BE.Monad.Utils
  ( mkAppConfigFromEnv
  , askConfig
  , throwAppError
  , catchAppError
  , handleAppError
  , withResource
  , hmacJwk
  , publishToRabbitMQ
  , withRabbitMQConnection
  ) where

import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Crypto.JOSE.Types
import Crypto.JWT
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Modulus.BE.DB.Internal.Config (mkConnectionPoolFromEnv)
import Modulus.BE.LLM.Providers (fetchAllProviders)
import Modulus.BE.Monad.AppM
import Modulus.BE.Monad.Error (AppError)
import Modulus.Common.Types
import qualified Network.AMQP as AMQP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Orville.PostgreSQL as O
import System.Environment (lookupEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import UnliftIO (SomeException, UnliftIO (unliftIO), askUnliftIO, bracket, try)

hmacJwk :: T.Text -> JWK
hmacJwk secret =
  fromKeyMaterial
    ( OctKeyMaterial . OctKeyParameters . Base64Octets $
        TE.encodeUtf8 secret
    )

-- | Create RabbitMQ connection from configuration
createRabbitMQConnection :: Text -> Text -> Text -> Text -> IO (Maybe AMQP.Connection)
createRabbitMQConnection host vhost login password = do
  result <- try $ AMQP.openConnection (T.unpack host) vhost login password
  case result of
    Left err -> do
      putStrLn $ "Failed to connect to RabbitMQ: " ++ show (err :: SomeException)
      pure Nothing
    Right conn -> do
      putStrLn "Successfully connected to RabbitMQ"
      pure (Just conn)

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
  -- RabbitMQ environment variables
  mbRabbitMQHost <- lookupEnv "MODULUS_RABBITMQ_HOST"
  mbRabbitMQVHost <- lookupEnv "MODULUS_RABBITMQ_VHOST"
  mbRabbitMQLogin <- lookupEnv "MODULUS_RABBITMQ_LOGIN"
  mbRabbitMQPassword <- lookupEnv "MODULUS_RABBITMQ_PASSWORD"
  apiTimeout <- readEnvWithDefault "MODULUS_API_TIMEOUT" 30
  loggerSet <- newStdoutLoggerSet defaultBufSize
  let env = fromMaybe Local (textToEnv (fromMaybe "" mbEnvironment))
  print ("ENVIRONMENT SET for " :: String, env)
  eConnectionPool <- mkConnectionPoolFromEnv
  case eConnectionPool of
    Left err -> pure $ Left (T.pack $ show err)
    Right connPool -> do
      case ( mbPort
           , mbJwtSecret
           , mbMailgunApi
           , mbUploadFilePath
           , mbRabbitMQHost
           , mbRabbitMQVHost
           , mbRabbitMQLogin
           , mbRabbitMQPassword
           ) of
        ( Just portStr
          , Just jwtSecret
          , Just mailGunApi
          , Just fileUploadPath
          , Just rabbitMQHost
          , Just rabbitMQVHost
          , Just rabbitMQLogin
          , Just rabbitMQPassword
          ) -> do
            case reads portStr of
              [(port, "")] -> do
                manager <- HTTP.newTlsManager
                modelProviders <- fetchAllProviders
                -- Create RabbitMQ connection
                rabbitMQConn <-
                  createRabbitMQConnection
                    (T.pack rabbitMQHost)
                    (T.pack rabbitMQVHost)
                    (T.pack rabbitMQLogin)
                    (T.pack rabbitMQPassword)
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
                      , -- RabbitMQ configuration
                        configRabbitMQConnection = rabbitMQConn
                      , configRabbitMQHost = T.pack rabbitMQHost
                      , configRabbitMQVirtualHost = T.pack rabbitMQVHost
                      , configRabbitMQLogin = T.pack rabbitMQLogin
                      , configRabbitMQPassword = T.pack rabbitMQPassword
                      }
              _ -> pure $ Left "Invalid PORT environment variable "
        (mb1, mb2, mb3, mb4, mb5, mb6, mb7, mb8) ->
          pure $
            Left $
              "Missing required environment variables: "
                <> mconcat
                  [ maybe "MODULUS_APP_PORT " (const "") mb1
                  , maybe "MODULUS_JWT_SECRET " (const "") mb2
                  , maybe "MODULUS_MAILGUN_API " (const "") mb3
                  , maybe "MODULUS_FILE_UPLOAD_PATH " (const "") mb4
                  , maybe "MODULUS_RABBITMQ_HOST " (const "") mb5
                  , maybe "MODULUS_RABBITMQ_VHOST " (const "") mb6
                  , maybe "MODULUS_RABBITMQ_LOGIN " (const "") mb7
                  , maybe "MODULUS_RABBITMQ_PASSWORD " (const "") mb8
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

-- | Utility function to publish a message to RabbitMQ if connection exists
publishToRabbitMQ ::
  (ToJSON a) =>
  Maybe AMQP.Connection ->
  String -> -- queue name
  a -> -- message to encode
  AppM ()
publishToRabbitMQ Nothing _ _ =
  liftIO $ putStrLn "No RabbitMQ connection available"
publishToRabbitMQ (Just conn) queueName message = do
  liftIO $ do
    channel <- AMQP.openChannel conn
    _ <-
      AMQP.declareQueue
        channel
        AMQP.newQueue
          { AMQP.queueName = T.pack queueName
          , AMQP.queueDurable = True
          }
    _ <-
      AMQP.publishMsg
        channel
        ""
        (T.pack queueName)
        AMQP.newMsg
          { AMQP.msgBody = encode message
          , AMQP.msgDeliveryMode = Just AMQP.Persistent
          }
    AMQP.closeChannel channel

-- | Execute an action with RabbitMQ connection if it exists
withRabbitMQConnection ::
  (AMQP.Connection -> AppM a) ->
  AppM (Maybe a)
withRabbitMQConnection action = do
  config <- askConfig
  case configRabbitMQConnection config of
    Nothing -> do
      liftIO $ putStrLn "No RabbitMQ connection available"
      pure Nothing
    Just conn -> Just <$> action conn
