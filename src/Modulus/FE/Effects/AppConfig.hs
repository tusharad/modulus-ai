{-# LANGUAGE GADTs #-}

module Modulus.FE.Effects.AppConfig
  ( AppConfigEff (..)
  , runAppConfigIO
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Modulus.Common.Types (AppConfig)

data AppConfigEff :: Effect where
  UseState :: AppConfigEff m AppConfig

type instance DispatchOf AppConfigEff = 'Dynamic

runAppConfigIO ::
  AppConfig ->
  Eff (AppConfigEff : es) a ->
  Eff es a
runAppConfigIO appCfg =
  interpret $ \_ -> \case
    UseState -> pure appCfg
