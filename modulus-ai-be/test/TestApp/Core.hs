module TestApp.Core (getTests) where

import TestApp.Common (TestData (..))

import qualified Data.Text as T
import Modulus.BE.Api.Server (appToServer)
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Handler.Auth (generateJWT)
import Modulus.BE.Monad.AppM (runAppM)
import Modulus.BE.Monad.Utils
import Modulus.Common.Types
import Orville.PostgreSQL (runOrvilleWithState)
import Test.Tasty
import TestApp.Conversation (conversationTest)
import TestApp.DB as DB
import TestApp.SampleData (insertData)

getTests :: IO (Either String [TestTree])
getTests = do
  eAppConf <- mkAppConfigFromEnv
  case eAppConf of
    Right appConf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState appConf) autoMigrateQ
      (uID1, uID2, convID, convTitle) <-
        runOrvilleWithState (configOrvilleState appConf) insertData
      -- Generate tokens for both users
      Right token1 <- runAppM appConf (generateJWT uID1)
      Right token2 <- runAppM appConf (generateJWT uID2)
      let testData =
            TestData
              { user1Token = token1
              , user2Token = token2
              , user1ConvID = convID
              , user1ConvTitle = convTitle
              }
      let app = appToServer appConf
      let t =
            [ DB.tests
            , conversationTest app testData
            ]
      pure $ Right t
    Left err -> pure $ Left (T.unpack err)
