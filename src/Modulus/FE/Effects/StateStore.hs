{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.FE.Effects.StateStore
  ( StateStore (..)
  , StreamState (..)
  , initStateStoreMap
  , SessionUUID (..)
  , StateStoreEff (..)
  , runStateStoreIO
  , useState
  , StateStoreMap
  , getStreamState
  , modifyState
  , getState
  ) where

import Control.Concurrent.MVar
import Data.Aeson
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Web.Hyperbole

data StreamState = InProgress Text | Complete Text
  deriving (Show, Eq)

data StateStore = StateStore
  { counter :: Int
  , currentPrompt :: Text
  , streamContent :: HM.Map Int StreamState
  }
  deriving (Show, Eq)

type StateStoreMap = MVar (HM.Map UUID (MVar StateStore))

initStateStoreMap :: IO StateStoreMap
initStateStoreMap = newMVar HM.empty

newtype SessionUUID = SessionUUID UUID
  deriving (Generic, ToJSON, FromJSON, FromParam, Read, ToParam, Show)

instance Session SessionUUID where
  sessionKey = "session-uuid"

data StateStoreEff :: Effect where
  UseState :: StateStoreEff m (MVar StateStore)

type instance DispatchOf StateStoreEff = 'Dynamic

runStateStoreIO ::
  (IOE :> es, Hyperbole :> es) =>
  StateStoreMap ->
  Eff (StateStoreEff : es) a ->
  Eff es a
runStateStoreIO globalMap =
  interpret $ \_ -> \case
    UseState -> getOrCreateStateStore globalMap

useState :: (StateStoreEff :> es) => Eff es (MVar StateStore)
useState = send UseState

getOrCreateStateStore ::
  (IOE :> es, Hyperbole :> es) =>
  StateStoreMap ->
  Eff es (MVar StateStore)
getOrCreateStateStore storeMap = do
  mbSid <- lookupSession @SessionUUID
  sid <- case mbSid of
    Nothing -> do
      newId <- liftIO UUID.nextRandom
      let s = SessionUUID newId
      saveSession s
      pure s
    Just sid -> pure sid
  let SessionUUID uuid = sid
  liftIO $ modifyMVar storeMap $ \hm -> do
    case HM.lookup uuid hm of
      Just stStore -> pure (hm, stStore)
      Nothing -> do
        x <-
          newMVar
            StateStore
              { counter = 0
              , currentPrompt = mempty
              , streamContent = HM.empty
              }
        pure (HM.insert uuid x hm, x)

modifyState ::
  (StateStoreEff :> es, IOE :> es) =>
  (StateStore -> StateStore) -> Eff es ()
modifyState f = do
  storeVar <- useState
  liftIO $ modifyMVar_ storeVar $ \s -> pure $ f s

getState :: (StateStoreEff :> es, IOE :> es) => Eff es StateStore
getState = liftIO . readMVar =<< useState

getStreamState ::
  (StateStoreEff :> es, IOE :> es) =>
  Int -> Eff es (Maybe StreamState)
getStreamState chatId = HM.lookup chatId . streamContent <$> getState
