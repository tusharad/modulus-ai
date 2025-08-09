{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Modulus.FE.Common.Types
  ( AppRoute (..)
  ) where

import GHC.Generics
import Web.Hyperbole
import Data.Text (Text)

data AppRoute
  = Main
  | Register
  | Verify
  | Login
  | Chat (Maybe Text)
  deriving (Eq, Generic, Show)

instance Route AppRoute where
  baseRoute = Just Main
