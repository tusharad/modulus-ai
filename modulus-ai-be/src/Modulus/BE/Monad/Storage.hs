module Modulus.BE.Monad.Storage
  ( Storage (..)
  , StorageConfig (..)
  , mkStorageFromEnv
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Modulus.BE.Monad.AppM (AppM)
import Modulus.Common.Types
import System.FilePath

class Storage handle where
  saveFile :: handle -> FilePath -> BSL.ByteString -> AppM (Either String ())

  loadFile :: handle -> FilePath -> AppM (Either String FilePath)

newtype StorageConfig = FileSystem FilePath
  deriving (Eq, Show)

instance Storage StorageConfig where
  saveFile (FileSystem uploadPath) fileName fileContent = do
    eRes <- liftIO $ try $ BSL.writeFile (uploadPath </> fileName) fileContent
    pure $ someExpToString eRes

  loadFile (FileSystem uploadPath) fileName = pure $ Right (uploadPath </> fileName)

someExpToString :: Either SomeException a -> Either String a
someExpToString (Left ex) = Left (show ex)
someExpToString (Right x) = Right x

mkStorageFromEnv :: AppM StorageConfig
mkStorageFromEnv = asks (FileSystem . configFileUploadPath)
