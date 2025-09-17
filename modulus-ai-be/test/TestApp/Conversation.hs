module TestApp.Conversation (conversationTest) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Langchain.LLM.Core as Langchain
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.Conversation (addConversation, addSummary, findSummaryByConvID)
import Modulus.BE.LLM
  ( AnyLLMProvider (AnyLLMProvider)
  , LLMProvider (..)
  , getOrCreateConversationSummary
  )
import Modulus.BE.Monad.AppM (runAppM)
import Modulus.Common.Types (AppConfig, ModelProviders)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Wai
import TestApp.Common (TestData (TestData, user1ConvID, user1Token))

conversationTest :: Application -> TestData -> AppConfig -> TestTree
conversationTest app testData appCfg =
  testGroup
    "Conversation API Tests"
    [ testGetModelProvidersAPI app
    , testGetConversationAPI app testData
    , testGetConversationMessagesAPI app testData
    , testCacheMiss appCfg
    , testCacheHit appCfg
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

data TestLLM = TestLLM
  { name :: String
  , mockSummary :: T.Text
  }
  deriving (Show, Eq)

instance LLMProvider TestLLM where
  summarizeOlderConversation llm _ = pure $ Right (mockSummary llm)
  streamResponse = undefined
  streamWithTools = undefined
  generateNewConversationTitle = undefined

testCacheMiss :: AppConfig -> TestTree
testCacheMiss appCfg = testCase "getOrCreateConversationSummary - should generate and cache a summary on miss" $ do
  eConv <- runAppM appCfg $ addConversation $ Conversation () () Nothing "Test" () ()
  conv <- case eConv of
    Left err -> assertFailure ("Failed to create conversation: " ++ show err) >> undefined
    Right c -> return c

  let convId = conversationID conv
      oldMessages = "User: what is 2+2\nAssistant: 2+2 is 4.\nUser: Thanks!\n"
      mockLLM = AnyLLMProvider $ TestLLM "test LLM" "This is the mock summary"

  eSummary <- runAppM appCfg $ getOrCreateConversationSummary mockLLM convId oldMessages
  case eSummary of
    Left err -> assertFailure ("Failed to generate summary: " ++ show err)
    Right msg ->
      liftIO $
        assertEqual
          "Should return the newly generated summary"
          (Right "This is the mock summary")
          (Langchain.content <$> msg)

  eMbCachedSummary <- runAppM appCfg $ findSummaryByConvID convId
  mbCachedSummary <- case eMbCachedSummary of
    Left err -> assertFailure ("Failed to query cached summary: " ++ show err) >> undefined
    Right mb -> pure mb
  case mbCachedSummary of
    Nothing -> liftIO $ assertFailure "Summary was not inserted into the cache"
    Just cached ->
      liftIO $
        assertEqual
          "Cached summary content is correct"
          "This is the mock summary"
          (oldConvSummarySummary cached)

testCacheHit :: AppConfig -> TestTree
testCacheHit appCfg = testCase "getOrCreateConversationSummary - should return cached summary on hit" $ do
  eConv <- runAppM appCfg $ addConversation $ Conversation () () Nothing "Test" () ()
  conv <- case eConv of
    Left err -> assertFailure ("Failed to create conversation: " ++ show err) >> undefined
    Right c -> return c
  let convId = conversationID conv
      oldMessages = "User: what is 2+2\nAssistant: 2+2 is 4.\nUser: Thanks!\n"
      cachedSummary = "This is the cached summary"
      mockLLM = AnyLLMProvider $ TestLLM "test LLM" "This is a different summary"
  _ <- runAppM appCfg $ addSummary $ OldConvSummary () convId cachedSummary ()
  eSummary <- runAppM appCfg $ getOrCreateConversationSummary mockLLM convId oldMessages
  case eSummary of
    Left err -> assertFailure ("Failed to get summary: " ++ show err)
    Right msg ->
      liftIO $
        assertEqual "Should return the cached summary" (Right cachedSummary) (Langchain.content <$> msg)
