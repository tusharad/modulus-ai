module TestApp.Common (TestData (..)) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

data TestData = TestData
  { user1Token :: BSL.ByteString
  , user2Token :: BSL.ByteString
  , user1ConvID :: BS.ByteString
  , user1ConvTitle :: T.Text
  , userPasswordChangeToken :: BSL.ByteString
  , userPasswordChangeEmail :: T.Text
  , userPasswordChangeNewPassword :: T.Text
  }
