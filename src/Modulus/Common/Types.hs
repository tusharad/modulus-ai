{-
`Modulus.Common` module stores types and function that shall be used by both FE and BE module.
 It is not necessary that every function and type that is used by both must be present here.
-}
{-# LANGUAGE GADTs #-}

module Modulus.Common.Types
  ( AppConfig (..)
  , LogLevel (..)
  , parseLogLevel
  , MinLogLevel
  , LogEntry (..)
  , AuthTokens (..)
  , StateStoreData (..)
  , Provider (..)
  ) where

import Data.Aeson hiding (Error)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import qualified Orville.PostgreSQL as O
import System.Log.FastLogger
import Web.Hyperbole
import Web.Hyperbole.Data.URI (Path (..))

-- | Application configuration
data AppConfig = AppConfig
  { configHttpManager :: HTTP.Manager -- TODO: We probably won't need this as
  -- we'll directly calling handler functions from Hyperbole
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

data AuthTokens = AuthTokens
  { accessToken :: Text
  , refreshToken :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Session AuthTokens where
  sessionKey = "authTokens"
  cookiePath = Just (Path True [])

instance Default AuthTokens where
  def = AuthTokens mempty mempty
data StateStoreData = StateStoreData
  { ollamaList :: [Text]
  , openrouterList :: [Text]
  }
  deriving (Show, Eq)

data Provider where
  OllamaProvider :: Text -> Provider
  OpenRouterProvider :: Text -> Text -> Provider

deriving instance Show Provider
deriving instance Eq Provider
deriving instance Generic Provider
deriving instance ToJSON Provider
deriving instance FromJSON Provider
