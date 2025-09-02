{-# LANGUAGE OverloadedStrings #-}

module Conversation (tests) where

import Modulus.Common.Types (AppConfig)
import Test.Tasty

tests :: AppConfig -> TestTree
tests _ =
  testGroup
    "Conversation Tests"
    []
