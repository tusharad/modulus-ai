{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module App.Common.Types
  ( AppRoute (..)
  , Provider (..)
  , AvailableTool (..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Web.Hyperbole

data AppRoute
  = Main
  | Chat (Maybe Int)
  deriving (Eq, Generic, Show)

instance Route AppRoute where
  baseRoute = Just Main

data Provider where
  OllamaProvider :: Text -> Provider
  OpenRouterProvider :: Text -> Text  -> Provider

deriving instance Show Provider
deriving instance Eq Provider
deriving instance Generic Provider
deriving instance ToJSON Provider
deriving instance FromJSON Provider

data AvailableTool = Web | Wiki
    deriving (Generic, ToJSON, Eq, Show, FromJSON, FromParam)
