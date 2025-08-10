{-# LANGUAGE ScopedTypeVariables #-}

module Modulus.Common.Utils
  ( runBE
  , runBEAuth
  , getUserOrGoToLogin
  , listAvailableOllamaModels 
  ) where

import Control.Exception (SomeException (..), try)
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (IOE)
import Modulus.BE.Auth.JwtAuthCombinator
import Modulus.BE.DB.Internal.Model (UserRead, User (userID))
import Modulus.BE.Log (logError, logDebug)
import Modulus.BE.Monad.AppM (AppM, runAppM)
import Modulus.BE.Monad.Error (AppError)
import Modulus.Common.Types (AuthTokens (..))
import Modulus.FE.Effects.AppConfig (AppConfigEff, getAppCfg)
import Modulus.FE.Utils (loginUrl)
import qualified Ollama
import Web.Hyperbole

getUserOrGoToLogin ::
  (AppConfigEff :> es, IOE :> es, Hyperbole :> es) =>
  AuthTokens ->
  Eff es UserRead
getUserOrGoToLogin authTokens = do
  appCfg <- getAppCfg
  eUserProfile <-
    liftIO $
      try $
        authenticateToken
          appCfg
          (BSL.fromStrict . TE.encodeUtf8 $ accessToken authTokens)
  case eUserProfile of
    Left (err :: SomeException) -> do
      void . runBE $ logError (T.pack $ show err)
      redirect loginUrl
    Right (Authenticated user) -> pure user
    Right _ -> redirect loginUrl

runBE ::
  (IOE :> es, AppConfigEff :> es) =>
  AppM a -> Eff es (Either AppError a)
runBE action = do
  cfg <- getAppCfg
  liftIO $ runAppM cfg action

runBEAuth ::
  (IOE :> es, AppConfigEff :> es, Hyperbole :> es) =>
  (AuthResult -> AppM a) -> Eff es (Either AppError a)
runBEAuth action = do
  cfg <- getAppCfg
  mbAuthTokens <- lookupSession @AuthTokens
  case mbAuthTokens of
    Nothing -> redirect loginUrl
    Just authTokens -> do
      user <- getUserOrGoToLogin authTokens
      _ <- liftIO $ runAppM cfg $ 
        logDebug $ "running request for user: " <> T.pack (show $ userID user)
      liftIO $ runAppM cfg (action (Authenticated user))

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
