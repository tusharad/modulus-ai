module TestApp.Conversation (conversationTest) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Modulus.BE.DB.Internal.Model
import Modulus.Common.Types (ModelProviders)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure)
import Test.Tasty.Wai
import TestApp.Common (TestData (TestData, user1ConvID, user1Token))

conversationTest :: Application -> TestData -> TestTree
conversationTest app testData =
  testGroup
    "Conversation API Tests"
    [ testGetModelProvidersAPI app
    , testGetConversationAPI app testData
    , testGetConversationMessagesAPI app testData
    ]

testGetConversationMessagesAPI :: Application -> TestData -> TestTree
testGetConversationMessagesAPI app TestData {..} = do
  testWai app "/conversations/{conversation_id} - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            GET
            ("/api/v1/conversations/" <> user1ConvID)
            ""
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> BSL.toStrict user1Token)
            ]
        )
    assertStatus' status200 res
    assertHeader "Content-Type" "application/json;charset=utf-8" res
    let eBody =
          Aeson.eitherDecode (simpleBody res) ::
            Either String [ChatMessageWithAttachments]
    case eBody of
      Left err -> liftIO $ assertFailure $ "Failed to decode response body: " ++ err
      Right _ -> return ()

testGetConversationAPI :: Application -> TestData -> TestTree
testGetConversationAPI app TestData {..} = do
  testWai app "/conversations - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            GET
            "/api/v1/conversations"
            ""
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> BSL.toStrict user1Token)
            ]
        )
    assertStatus' status200 res
    assertHeader "Content-Type" "application/json;charset=utf-8" res
    let eBody =
          Aeson.eitherDecode (simpleBody res) ::
            Either String [ConversationRead]
    case eBody of
      Left err -> liftIO $ assertFailure $ "Failed to decode response body: " ++ err
      Right _ -> return ()

testGetModelProvidersAPI :: Application -> TestTree
testGetModelProvidersAPI app = do
  testWai app "/model_providers - 200" $ do
    res <-
      srequest
        ( buildRequestWithHeaders
            GET
            "/api/v1/conversations/model_providers"
            ""
            [("Content-Type", "application/json")]
        )
    assertStatus' status200 res
    assertHeader "Content-Type" "application/json;charset=utf-8" res
    let eBody =
          Aeson.eitherDecode (simpleBody res) ::
            Either String [ModelProviders]
    case eBody of
      Left err -> liftIO $ assertFailure $ "Failed to decode response body: " ++ err
      Right providers ->
        liftIO $
          assertBool
            "The list of providers should not be empty"
            (not $ null providers)
