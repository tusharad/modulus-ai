module TestApp.Auth (authTests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Modulus.BE.Api.Types
import Modulus.BE.DB.Internal.Model (ApiKeyRead)
import Modulus.BE.DB.Queries.ApiKeys (getApiKeyByUserIdAndProviderName)
import Modulus.BE.Monad.AppM (runAppM)
import Modulus.Common.Types (AppConfig)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure)
import Test.Tasty.Wai
import TestApp.Common (TestData (..))

authTests :: Application -> TestData -> AppConfig -> TestTree
authTests app testData appCfg =
  testGroup
    "Authentication Tests"
    [ testPasswordChange app testData
    , sequentialTestGroup
        "API Key Tests"
        AllSucceed
        [ testAddApiKey app testData appCfg
        , testGetApiKeys app testData appCfg
        ]
    ]

testGetApiKeys :: Application -> TestData -> AppConfig -> TestTree
testGetApiKeys app TestData {..} _ =
  testWai app "/auth/api-keys - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            GET
            "/api/v1/auth/api-keys"
            ""
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> BSL.toStrict user1Token)
            ]
        )
    liftIO $ putStrLn ("Get API Keys response: " ++ show res)
    assertStatus' status200 res
    assertHeader "Content-Type" "application/json;charset=utf-8" res
    liftIO $ do
      let mbApiKeys = decode (simpleBody res) :: Maybe [ApiKeyRead]
      case mbApiKeys of
        Nothing -> assertFailure "Failed to decode API keys response"
        Just apiKeys -> assertBool "At least one API key found" (not (null apiKeys))

-- Adding new key shall reflect new data in table
testAddApiKey :: Application -> TestData -> AppConfig -> TestTree
testAddApiKey app TestData {..} appCfg =
  testWai app "/auth/api-key - 200" $ do
    let addApiKeyPayload = encode $ AddApiKeyRequest {providerName = "gemini", keyVal = "some-api-key"}
    res <-
      srequest
        ( buildRequestWithHeaders
            POST
            "/api/v1/auth/api-keys"
            addApiKeyPayload
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> BSL.toStrict user1Token)
            ]
        )
    liftIO $ putStrLn ("Add API Key response: " ++ show res)
    assertStatus' status200 res
    assertHeader "Content-Type" "application/json;charset=utf-8" res
    liftIO $ do
      eApiKey <- runAppM appCfg $ getApiKeyByUserIdAndProviderName user1ID "gemini"
      case eApiKey of
        Left err -> assertFailure ("Failed to add apiKey: " ++ show err)
        Right mbApiKeyRead -> assertBool "ApiKey found after addition" (isJust mbApiKeyRead)

testPasswordChange :: Application -> TestData -> TestTree
testPasswordChange app TestData {..} = do
  -- run tests in squence
  sequentialTestGroup "Password Change Tests" AllSucceed [changePass, checkLoginNewPass]
  where
    checkLoginNewPass =
      testWai app "/auth/login - 200" $ do
        let loginPayload =
              encode $
                LoginRequest
                  { loginEmail = userPasswordChangeEmail
                  , loginPassword = userPasswordChangeNewPassword
                  }
        res <-
          srequest
            ( buildRequestWithHeaders
                POST
                "/api/v1/auth/login"
                loginPayload
                [ ("Content-Type", "application/json")
                ]
            )
        assertStatus' status200 res
        assertHeader "Content-Type" "application/json;charset=utf-8" res
    changePass =
      let passwordChangeRequestPayload =
            encode $
              ChangePasswordRequest
                { oldPassword = "user3pass"
                , newPassword = userPasswordChangeNewPassword
                , confirmNewPassword = userPasswordChangeNewPassword
                }
       in testWai app "/auth/password-change - 200" $ do
            res <-
              srequest
                ( buildRequestWithHeaders
                    PUT
                    "/api/v1/auth/change-password"
                    passwordChangeRequestPayload
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> BSL.toStrict userPasswordChangeToken)
                    ]
                )
            assertStatus' status200 res
            assertHeader "Content-Type" "application/json;charset=utf-8" res
