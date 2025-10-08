module Modulus.BE.Monad.Storage
  ( Storage (..)
  , StorageConfig (..)
  , mkStorageFromEnv
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Langchain.Error (LangchainError, fromString)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.Common.Types
import System.FilePath

class Storage handle where
  saveFile :: handle -> FilePath -> BSL.ByteString -> AppM (Either LangchainError ())

  loadFile :: handle -> FilePath -> AppM (Either LangchainError FilePath)

newtype StorageConfig = FileSystem FilePath
  deriving (Eq, Show)

instance Storage StorageConfig where
  saveFile (FileSystem uploadPath) fileName fileContent = do
    eRes <- liftIO $ try $ BSL.writeFile (uploadPath </> fileName) fileContent
    pure $ someExpToError eRes

  loadFile (FileSystem uploadPath) fileName = pure $ Right (uploadPath </> fileName)

someExpToError :: Either SomeException a -> Either LangchainError a
someExpToError (Left ex) = Left (fromString $ show ex)
someExpToError (Right x) = Right x

mkStorageFromEnv :: AppM StorageConfig
mkStorageFromEnv = asks (FileSystem . configFileUploadPath)
