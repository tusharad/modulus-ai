module Modulus.BE.Log
  ( logDebug
  , logError
  , logInfo
  , logWarn
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time
import Modulus.BE.Monad.AppM
import System.Log.FastLogger
import Modulus.BE.Common.Types
import Modulus.BE.Monad.Utils (askConfig)
import Data.Aeson (encode)

logDebug :: Text -> AppM ()
logDebug msg = do
  res <- askConfig
  liftIO $ logger (configLoggerSet res) (configMinLogLevel res) Debug msg

logError :: Text -> AppM ()
logError msg = do
  res <- askConfig
  liftIO $ logger (configLoggerSet res) (configMinLogLevel res) Error msg

logInfo :: Text -> AppM ()
logInfo msg = do
  res <- askConfig
  liftIO $ logger (configLoggerSet res) (configMinLogLevel res) Info msg

logWarn :: Text -> AppM ()
logWarn msg = do
  res <- askConfig
  liftIO $ logger (configLoggerSet res) (configMinLogLevel res) Warn msg

logger :: LoggerSet -> MinLogLevel -> LogLevel -> Text -> IO ()
logger loggerSet_ minLogLevel_ logLevel0 msg = do
  when (logLevel0 >= minLogLevel_) $ do
    currTime <- getCurrentTime
    let logEntry = LogEntry {
        logTimestamp = currTime
      , logLevel = logLevel0
      , logMessage = msg
    }
    pushLogStrLn loggerSet_ . toLogStr $ encode logEntry
