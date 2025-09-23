module TestApp.Auth (authTests) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Modulus.BE.Api.Types
import Modulus.Common.Types (AppConfig)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Tasty
import Test.Tasty.Wai
import TestApp.Common (TestData (..))

authTests :: Application -> TestData -> AppConfig -> TestTree
authTests app testData _ =
  testGroup
    "Authentication Tests"
    [ testPasswordChange app testData
    ]

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
