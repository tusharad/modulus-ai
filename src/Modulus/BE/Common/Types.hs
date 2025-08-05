{-# LANGUAGE OverloadedStrings #-}

module Modulus.BE.Common.Types
  ( LogLevel (..)
  , parseLogLevel
  , MinLogLevel
  , LogEntry (..)
  ) where

import Data.Aeson hiding (Error)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

---------LOGS-----------------
-- | Log severity levels
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Enum, Bounded)

type MinLogLevel = LogLevel

instance ToJSON LogLevel where
  toJSON = toJSON . logLevelToText

-- | Convert LogLevel to Text representation
logLevelToText :: LogLevel -> Text
logLevelToText Debug = "DEBUG"
logLevelToText Info = "INFO"
logLevelToText Warn = "WARN"
logLevelToText Error = "ERROR"

-- | Parse LogLevel from Text (case-insensitive)
logLevelFromText :: Text -> Maybe LogLevel
logLevelFromText t = case T.toUpper t of
  "DEBUG" -> Just Debug
  "INFO" -> Just Info
  "WARN" -> Just Warn
  "ERROR" -> Just Error
  _ -> Nothing

-- | Parse LogLevel from String (for environment variables)
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel = logLevelFromText . T.pack

-- | Structured log entry
data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel :: LogLevel
  , logMessage :: Text
  }
  deriving (Show, Eq)

instance ToJSON LogEntry where
  toJSON entry =
    object
      [ "timestamp" .= logTimestamp entry
      , "level" .= logLevel entry
      , "message" .= logMessage entry
      ]
---------LOGS-----------------
