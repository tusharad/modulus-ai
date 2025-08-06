{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-
`AppM` is the custom monad stack for all things of Modulus backend.
It will have attached:
    1. Servant Monad: to deal with servant api.
    2. OrvilleState Monad: to deal with database operation of orville ORM.
    3. MonadLogger: deal with logging.
-}
module Modulus.BE.Monad.AppM
  ( AppM (..)
  , AppConfig (..)
  , appErrorToServerError
  , appMToHandler
  , runAppM
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import GHC.Generics
import Modulus.BE.Common.Types
import Modulus.BE.Monad.Error
import qualified Network.HTTP.Client as HTTP
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.UnliftIO as OrvilleUnliftIO
import Servant
import System.Log.FastLogger
import UnliftIO

newtype MyExceptT e m a = MyExceptT
  { runMyExceptT :: ExceptT e m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError e
    )

-- | Application configuration
data AppConfig = AppConfig
  { configHttpManager :: HTTP.Manager
  , configPort :: Int
  , configLogLevel :: Text
  , configEnvironment :: Text -- "development", "staging", "production"
  , configRedisUrl :: Maybe Text
  , configJwtSecret :: Text
  , configExternalApiTimeout :: Int -- seconds
  , configLoggerSet :: LoggerSet
  , configMinLogLevel :: MinLogLevel
  , configOrvilleState :: O.OrvilleState
  , configMailGunApiKey :: Text
  }
  deriving (Generic)

{- | The main application monad stack
  ReaderT for configuration (immutable)
  StateT for mutable application state
  ExceptT for error handling
  IO as the base monad
-}
newtype AppM a = AppM
  { unAppM :: ReaderT AppConfig (MyExceptT AppError IO) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppConfig
    , MonadError AppError
    )

-- | HasOrvilleState instance for Orville PostgreSQL integration
instance O.HasOrvilleState AppM where
  askOrvilleState :: AppM O.OrvilleState
  askOrvilleState = asks configOrvilleState

  localOrvilleState :: (O.OrvilleState -> O.OrvilleState) -> AppM a -> AppM a
  localOrvilleState f (AppM reader_) = do
    AppM $
      local
        ( \myAppState ->
            myAppState {configOrvilleState = f (configOrvilleState myAppState)}
        )
        reader_

-- | MonadOrvilleControl instance for Orville resource management
instance O.MonadOrvilleControl AppM where
  liftWithConnection ::
    (forall a. (O.Connection -> IO a) -> IO a) ->
    (O.Connection -> AppM b) ->
    AppM b
  liftWithConnection = OrvilleUnliftIO.liftWithConnectionViaUnliftIO

  liftCatch = OrvilleUnliftIO.liftCatchViaUnliftIO

  liftMask = OrvilleUnliftIO.liftMaskViaUnliftIO

-- | MonadOrville instance - provides the complete Orville functionality
instance O.MonadOrville AppM

-- | UnliftIO instance for AppM (critical for resource management)
instance MonadUnliftIO AppM where
  withRunInIO :: ((forall a. AppM a -> IO a) -> IO b) -> AppM b
  withRunInIO inner = AppM $ do
    config <- asks id
    liftIO $ inner $ \action -> do
      result <-
        runExceptT . runMyExceptT $
          runReaderT (unAppM action) config
      case result of
        Left err -> throwIO err
        Right a -> pure a

-- | Convert AppError to ServerError for Servant
appErrorToServerError :: AppError -> ServerError
appErrorToServerError (ValidationError msg) =
  err400 {errBody = encodeError msg}
appErrorToServerError (NotFoundError msg) =
  err404 {errBody = encodeError msg}
appErrorToServerError (AuthenticationError msg) =
  err401 {errBody = encodeError msg}
appErrorToServerError (AuthorizationError msg) =
  err403 {errBody = encodeError msg}
appErrorToServerError (DatabaseError msg) =
  err500 {errBody = encodeError msg}
appErrorToServerError (ExternalServiceError msg) =
  err502 {errBody = encodeError msg}
appErrorToServerError (ConfigurationError msg) =
  err500 {errBody = encodeError msg}
appErrorToServerError (InternalError msg _) =
  err500 {errBody = encodeError msg}

encodeError :: Text -> BSL.ByteString
encodeError = Aeson.encode . Aeson.object . pure . ("error" Aeson..=)

-- | Convert AppM to Servant Handler
appMToHandler :: AppConfig -> AppM a -> Servant.Handler a
appMToHandler config action = do
  result <- liftIO $ runExceptT . runMyExceptT $ runReaderT (unAppM action) config
  case result of
    Left err -> throwError $ appErrorToServerError err
    Right a -> pure a

runAppM :: AppConfig -> AppM a -> IO a
runAppM cfg action = do
  result <- runExceptT . runMyExceptT $ runReaderT (unAppM action) cfg
  case result of
    Left err -> error $ "AppM failed during jwtAuthCheck: " <> show err
    Right val -> pure val
