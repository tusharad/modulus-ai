module Modulus.BE.Monad.Storage
  ( Storage (..)
  , StorageConfig (..)
  , mkStorageFromEnv
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Google.Cloud.Storage.Bucket (downloadObject, uploadObject)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.Common.Types
import System.FilePath

class Storage handle where
  saveFile :: handle -> FilePath -> BSL.ByteString -> AppM (Either String ())

  loadFile :: handle -> FilePath -> AppM (Either String FilePath)

data StorageConfig = FileSystem FilePath | StorageBucket Text
  deriving (Eq, Show)

instance Storage StorageConfig where
  saveFile (FileSystem uploadPath) fileName fileContent = do
    eRes <- liftIO $ try $ BSL.writeFile (uploadPath </> fileName) fileContent
    pure $ someExpToString eRes
  saveFile (StorageBucket bucketName) objectName fileContent =
    liftIO $ uploadObject (T.unpack bucketName) objectName fileContent

  loadFile (FileSystem uploadPath) fileName = pure $ Right (uploadPath </> fileName)
  loadFile (StorageBucket bucketName) objectName = do
    eRes <- liftIO $ downloadObject (T.unpack bucketName) objectName
    case eRes of
      Left err -> pure $ Left err
      Right r -> do
        uploadPath <- asks configFileUploadPath
        let fPath = uploadPath </> objectName
        eFileWritten <- liftIO $ try $ BSL.writeFile fPath r
        case eFileWritten of
          Left err -> pure $ Left (show (err :: SomeException))
          Right _ -> pure $ Right fPath

someExpToString :: Either SomeException a -> Either String a
someExpToString (Left ex) = Left (show ex)
someExpToString (Right x) = Right x

mkStorageFromEnv :: AppM StorageConfig
mkStorageFromEnv = do
  env <- asks configEnvironment
  case env of
    Local -> asks (FileSystem . configFileUploadPath)
    _ -> asks (StorageBucket . configGCPBucketName)
