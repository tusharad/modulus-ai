{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-- LLM related functions shall be here
module Modulus.BE.LLM
  ( attachDocumentRAG
  , runOpenRouterWithTools
  , runOllamaWithTools
  , streamWithProvider
  , mkLLMProvider
  , LLMProvider (..)
  , AnyLLMProvider (..)
  , NewConversationTitle (..)
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Ollama.Chat
  ( FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  )
import qualified Data.Ollama.Chat as Ollama
import qualified Data.Ollama.Common.SchemaBuilder as Ollama
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Langchain.DocumentLoader.Core (BaseLoader (load))
import Langchain.DocumentLoader.FileLoader (FileLoader (FileLoader))
import Langchain.DocumentLoader.PdfLoader (PdfLoader (PdfLoader))
import qualified Langchain.LLM.Core as L
import qualified Langchain.LLM.Core as Langchain
import Langchain.LLM.Gemini as LLMGemini
import qualified Langchain.LLM.Internal.OpenAI as Internal
import qualified Langchain.LLM.Internal.OpenAI as OpenAIInternal
import qualified Langchain.LLM.Internal.SchemaBuilder as LangchainSchema
import Langchain.LLM.Ollama as LLMOllama
import Langchain.LLM.OpenAICompatible as OpenAILike hiding (metadata)
import Langchain.PromptTemplate
import Langchain.Tool.Core (Tool (runTool))
import Langchain.Tool.WebScraper (WebScraper (WebScraper))
import Langchain.Tool.WikipediaTool (defaultWikipediaTool)
import Modulus.BE.Api.Types (LLMRespStreamBody (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.DocumentEmbedding (getDocumentEmbeddingsByAttachmentId)
import Modulus.BE.LLM.Embeddings (getRelevantContext, storeDocsForEmbeddings)
import Modulus.BE.Log (logDebug, logError)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Storage
import System.FilePath

-- | Create tool message from tool result
createToolMessage :: (String, Text) -> Message
createToolMessage (functionName, result) =
  Message
    System
    ("Tool (" <> T.pack functionName <> ") result: " <> result)
    defaultMessageData

attachDocumentRAG ::
  [MessageAttachmentRead] ->
  NE.NonEmpty L.Message ->
  LLMRespStreamBody ->
  AppM (NE.NonEmpty L.Message)
attachDocumentRAG msgAttachmentsList msgList_ respStreamBody = do
  storageConfig <- mkStorageFromEnv
  eDocs <- runExceptT $ do
    perAttachmentDocs <-
      mapM
        ( \MessageAttachment {..} -> do
            existing <-
              ExceptT $
                Right
                  <$> getDocumentEmbeddingsByAttachmentId messageAttachmentID
            case existing of
              [] -> do
                let attName = T.unpack messageAttachmentFileName
                fPath <- ExceptT $ loadFile storageConfig attName
                docs <-
                  if ".pdf" == takeExtension fPath
                    then do
                      let sourcePdf = PdfLoader fPath
                      ExceptT $ liftIO $ load sourcePdf
                    else do
                      let source = FileLoader fPath
                      ExceptT $ liftIO $ load source
                ExceptT $ Right <$> logDebug "storing new docs"
                ExceptT $ storeDocsForEmbeddings respStreamBody messageAttachmentID docs
              _ -> pure ()
        )
        msgAttachmentsList
    pure $ mconcat perAttachmentDocs
  case eDocs of
    Left err -> do
      logDebug $ "Error while generating docs" <> T.pack err
      pure msgList_
    Right _ -> do
      let lastUserMsg = Langchain.content $ NE.last msgList_
      eSysPrompt <- runExceptT $ do
        ExceptT $ Right <$> logDebug "getting relevant context"
        context <- ExceptT $ getRelevantContext respStreamBody lastUserMsg
        ExceptT . pure $
          renderPrompt
            (PromptTemplate systemTemplate)
            (HM.fromList [("context", context)])
      case eSysPrompt of
        Left err -> do
          logError $ "Error while getting relevant docs" <> T.pack err
          pure msgList_
        Right sysPrompt -> do
          let sysMsg = L.Message L.System sysPrompt L.defaultMessageData
          let userMsg = L.Message L.User lastUserMsg L.defaultMessageData
          logDebug $ "sysPrompt" <> L.content sysMsg
          pure $ NE.fromList $ NE.init msgList_ ++ [sysMsg, userMsg]

systemTemplate :: Text
systemTemplate =
  T.unlines
    [ "Use the following pieces of context to answer the user's question."
    , "If you don't know the answer, just say that you don't know, \
      \don't try to make up an answer."
    , "{context}"
    ]

runOpenRouterWithTools ::
  OpenAICompatible ->
  NE.NonEmpty Message ->
  StreamHandler Text ->
  Text ->
  IO (Either String ())
runOpenRouterWithTools llm msgList sh tool = do
  let toolDefs = getOpenRouterToolDefinitions tool
      openAIParams = defaultOpenAIParams {OpenAILike.tools = Just toolDefs}

  -- First call to get tool calls
  eRes <- chat llm msgList (Just openAIParams)
  case eRes of
    Left err -> pure $ Left $ "Error from chat: " ++ show err
    Right response -> do
      case toolCalls (messageData response) of
        Nothing -> do
          -- No tool calls, proceed with normal streaming
          void $ stream llm msgList sh Nothing
          pure $ Right ()
        Just toolCallList -> do
          -- Execute tool calls and add results to message list
          toolResults <- mapM (executeOpenRouterToolCall tool) toolCallList
          print ("tool result" :: String, toolResults)
          let toolMessages = map createToolMessage toolResults
              updatedMsgList = msgList <> NE.fromList toolMessages
          -- Stream the final response
          stream llm updatedMsgList sh Nothing

-- TODO: These should be Haskell types
getOpenRouterToolDefinitions :: Text -> [OpenAIInternal.InputTool]
getOpenRouterToolDefinitions = \case
  "WebSearch" -> [openRouterWebScraperTool]
  "Wikipedia" -> [openRouterWikiSearchTool]
  _ -> [] -- TODO: There should be Haskell type instead of plain Text

openRouterWebScraperTool :: OpenAIInternal.InputTool
openRouterWebScraperTool =
  OpenAIInternal.InputTool
    { toolType = "function"
    , function =
        OpenAIInternal.FunctionDef
          { functionName = "webScraper"
          , functionDescription =
              Just
                "Scrapes content from a webpage. Provide a valid URL."
          , functionParameters =
              Just $
                OpenAIInternal.FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["url"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "url"
                            , OpenAIInternal.FunctionParameters
                                "string"
                                Nothing
                                Nothing
                                Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

openRouterWikiSearchTool :: OpenAIInternal.InputTool
openRouterWikiSearchTool =
  OpenAIInternal.InputTool
    { toolType = "function"
    , function =
        OpenAIInternal.FunctionDef
          { functionName = "searchWiki"
          , functionDescription = Just "Search Wikipedia for information"
          , functionParameters =
              Just $
                OpenAIInternal.FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["query"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "query"
                            , OpenAIInternal.FunctionParameters
                                "string"
                                Nothing
                                Nothing
                                Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

executeOpenRouterToolCall :: Text -> ToolCall -> IO (String, Text)
executeOpenRouterToolCall tool (ToolCall _ _ toolFunction) = do
  let functionName = toolFunctionName toolFunction
      arguments = toolFunctionArguments toolFunction
  case tool of
    "WebSearch" -> do
      case HM.lookup "url" arguments of
        Just (String url) -> do
          result <- runTool WebScraper url
          case result of
            Left err -> pure (T.unpack functionName, T.pack err)
            Right content -> pure (T.unpack functionName, content)
        _ -> pure (T.unpack functionName, "Error: url parameter not found")
    "Wikipedia" -> do
      case HM.lookup "query" arguments of
        Just (String query) -> do
          result <- runTool defaultWikipediaTool query
          pure (T.unpack functionName, result)
        _ -> pure (T.unpack functionName, "Error: query parameter not found")
    _ -> pure ("", "Error: Unknown tool")

-------------------Ollama Tool call -------------
runOllamaWithTools ::
  Ollama ->
  NE.NonEmpty Message ->
  StreamHandler Text ->
  Text ->
  IO (Either String ())
runOllamaWithTools llm msgList sh tool = do
  let toolDefs = getToolDefinitions tool
      ollamaParams = defaultOllamaParams {LLMOllama.tools = Just toolDefs}
  -- First call to get tool calls
  eRes <- chat llm msgList (Just ollamaParams)
  case eRes of
    Left err -> pure $ Left $ "Error from chat: " ++ show err
    Right response -> do
      case toolCalls (messageData response) of
        Nothing -> do
          -- No tool calls, proceed with normal streaming
          print ("no tools were called" :: String)
          void $ stream llm msgList sh Nothing
          pure $ Right ()
        Just toolCallList -> do
          -- Execute tool calls and add results to message list
          toolResults <- mapM (executeToolCallFromResponse tool) toolCallList
          print ("tool result" :: String, toolResults)
          let toolMessages = map createToolMessage toolResults
              updatedMsgList = msgList <> NE.fromList toolMessages
          -- Stream the final response
          void $ stream llm updatedMsgList sh Nothing
          pure $ Right ()

getToolDefinitions :: Text -> [InputTool]
getToolDefinitions = \case
  "WebSearch" -> [ollamaWebScraperTool]
  "Wikipedia" -> [wikiSearchTool]
  _ -> [] -- TODO:

ollamaWebScraperTool :: InputTool
ollamaWebScraperTool =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = "webScraper"
          , functionDescription =
              Just
                "Scrapes content from a webpage. Provide a valid URL."
          , functionParameters =
              Just $
                FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["url"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [("url", FunctionParameters "string" Nothing Nothing Nothing)]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

wikiSearchTool :: InputTool
wikiSearchTool =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = "searchWiki"
          , functionDescription = Just "Search Wikipedia for information"
          , functionParameters =
              Just $
                FunctionParameters
                  { parameterType = "object"
                  , requiredParams = Just ["query"]
                  , parameterProperties =
                      Just $
                        HM.fromList
                          [
                            ( "query"
                            , FunctionParameters
                                "string"
                                Nothing
                                Nothing
                                Nothing
                            )
                          ]
                  , additionalProperties = Just False
                  }
          , functionStrict = Nothing
          }
    }

executeToolCallFromResponse :: Text -> ToolCall -> IO (String, Text)
executeToolCallFromResponse tool (ToolCall _ _ toolFunction) = do
  let functionName = toolFunctionName toolFunction
      arguments = toolFunctionArguments toolFunction

  case tool of
    "WebSearch" -> do
      case HM.lookup "url" arguments of
        Just (String url) -> do
          result <- runTool WebScraper url
          case result of
            Left err -> pure (T.unpack functionName, T.pack err)
            Right content -> pure (T.unpack functionName, content)
        _ -> pure (T.unpack functionName, "Error: url parameter not found")
    "Wikipedia" -> do
      case HM.lookup "query" arguments of
        Just (String query) -> do
          result <- runTool defaultWikipediaTool query
          pure (T.unpack functionName, result)
        _ -> pure (T.unpack functionName, "Error: query parameter not found")
    _ -> pure ("", "Error: unknown toolName")

class LLMProvider a where
  streamResponse ::
    a ->
    NE.NonEmpty Langchain.Message ->
    StreamHandler Text ->
    Maybe (Langchain.LLMParams a) ->
    IO (Either String ())

  streamWithTools ::
    a ->
    NE.NonEmpty Langchain.Message ->
    StreamHandler Text ->
    Text ->
    IO (Either String ())

  generateNewConversationTitle :: a -> Text -> IO (Either String NewConversationTitle)

data AnyLLMProvider where
  AnyLLMProvider :: (LLMProvider a) => a -> AnyLLMProvider

streamWithProvider ::
  AnyLLMProvider ->
  NE.NonEmpty Langchain.Message ->
  StreamHandler Text ->
  Maybe Text ->
  IO (Either String ())
streamWithProvider (AnyLLMProvider provider) msgs sh Nothing =
  streamResponse provider msgs sh Nothing
streamWithProvider (AnyLLMProvider provider) msgs sh (Just tool) =
  streamWithTools provider msgs sh tool

instance LLMProvider Ollama where
  streamResponse = Langchain.stream
  streamWithTools = runOllamaWithTools
  generateNewConversationTitle l userQuestion = do
    let schema =
          Ollama.buildSchema $
            Ollama.emptyObject
              Ollama.|+ ("title", Ollama.JString)
              Ollama.|! "title"
    let msgContent = generateTitlePrompt <> userQuestion
    let msg = NE.fromList [Message OpenAILike.User msgContent defaultMessageData]
    let ollamaParams =
          defaultOllamaParams
            { format = Just $ Ollama.SchemaFormat schema
            }
    fmap (toNewTitle . content) <$> chat l msg (Just ollamaParams)

instance LLMProvider OpenAICompatible where
  streamResponse = Langchain.stream
  streamWithTools = runOpenRouterWithTools
  generateNewConversationTitle l userQuestion = do
    let schema =
          LangchainSchema.buildSchema $
            LangchainSchema.emptyObject
              LangchainSchema.|+ ("title", LangchainSchema.JString)
              LangchainSchema.|! "title"
    let msgContent = generateTitlePrompt <> userQuestion
    let msg = NE.fromList [Message OpenAILike.User msgContent defaultMessageData]
    let params =
          defaultOpenAIParams
            { responseFormat =
                Just $
                  Internal.JsonSchemaFormat "SomeSchema" schema False
            }
    fmap (toNewTitle . content) <$> chat l msg (Just params)

instance LLMProvider Gemini where
  streamResponse = Langchain.stream
  streamWithTools = runOpenRouterWithTools . geminiToOpenAICompatible
  generateNewConversationTitle =
    generateNewConversationTitle . geminiToOpenAICompatible

geminiToOpenAICompatible :: Gemini -> OpenAICompatible
geminiToOpenAICompatible Gemini {..} =
  OpenAILike.OpenAICompatible
    { OpenAILike.apiKey = Just apiKey
    , modelName = geminiModelName
    , callbacks = []
    , baseUrl = baseUrl
    , defaultBaseUrl = "https://generativelanguage.googleapis.com/v1beta/openai"
    , providerName = "gemini"
    }

mkLLMProvider :: LLMRespStreamBody -> AppM (Either Text AnyLLMProvider)
mkLLMProvider LLMRespStreamBody {..} = pure $ case provider of
  "ollama" ->
    let llm = Ollama modelUsed []
     in Right $ AnyLLMProvider llm
  "openrouter" ->
    let llm = mkOpenRouter modelUsed [] Nothing (fromMaybe "" apiKey)
     in Right $ AnyLLMProvider llm
  "gemini" ->
    let llm =
          Gemini
            { apiKey = fromMaybe "" apiKey
            , geminiModelName = modelUsed
            , callbacks = []
            , baseUrl = Nothing
            }
     in Right $ AnyLLMProvider llm
  _ -> Left $ "Provider not found: " <> provider

newtype NewConversationTitle = NewConversationTitle {title :: Text}
  deriving (Show, Eq, Generic, FromJSON)

toNewTitle :: Text -> NewConversationTitle
toNewTitle txt =
  fromMaybe (NewConversationTitle "New Chat") $
    decode (BS.fromStrict $ TE.encodeUtf8 txt)

generateTitlePrompt :: Text
generateTitlePrompt =
  T.unlines
    [ "You are given a user question, write a 5 word summary of user's question for"
    , "the conversation title. Follow the json structure only"
    , "User question: "
    ]
