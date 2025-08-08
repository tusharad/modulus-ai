{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Modulus.FE.Common.Types
  ( AppRoute (..)
  ) where

import GHC.Generics
import Web.Hyperbole

data AppRoute
  = Main
  | Register
  | Verify
  | Chat (Maybe Int)
  deriving (Eq, Generic, Show)

instance Route AppRoute where
  baseRoute = Just Main
