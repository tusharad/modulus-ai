{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-- LLM related functions shall be here
module Modulus.BE.LLM
  ( attachDocumentRAG
  , runOpenRouterWithTools
  , runOllamaWithTools
  , streamWithProvider
  , mkLLMProvider
  , summarizeConversationHistory
  , LLMProvider (..)
  , AnyLLMProvider (..)
  , NewConversationTitle (..)
  , getOrCreateConversationSummary
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as HM
import Data.Maybe (fromMaybe, listToMaybe)
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
import Langchain.Error (LangchainError, fromString)
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
import Langchain.Utils (showText)
import Modulus.BE.Api.Types (LLMRespStreamBody (..))
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.Conversation (addSummary, findSummaryByConvID)
import Modulus.BE.DB.Queries.DocumentEmbedding (getDocumentEmbeddingsByAttachmentId)
import Modulus.BE.LLM.Embeddings (getRelevantContext, storeDocsForEmbeddings)
import Modulus.BE.Log (logDebug, logError)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Storage
import System.FilePath
import UnliftIO.Async (mapConcurrently)

-- | Create tool message from tool result
createToolMessage :: (String, Text) -> Message
createToolMessage (functionName, result) =
  Message
    System
    ("Tool (" <> T.pack functionName <> ") result: " <> result)
    defaultMessageData

storeDocIfNotExist ::
  Storage handle =>
  handle ->
  LLMRespStreamBody ->
  MessageAttachment MessageAttachmentID b ->
  AppM (Either LangchainError ())
storeDocIfNotExist storageConfig respStreamBody MessageAttachment {..} = do
  runExceptT $ do
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
        ExceptT $
          storeDocsForEmbeddings
            respStreamBody
            messageAttachmentID
            docs
      _ -> pure ()

attachDocumentRAG ::
  [MessageAttachmentRead] ->
  NE.NonEmpty L.Message ->
  LLMRespStreamBody ->
  AppM (NE.NonEmpty L.Message)
attachDocumentRAG msgAttachmentsList msgList_ respStreamBody = do
  storageConfig <- mkStorageFromEnv
  (errs, _) <-
    partitionEithers
      <$> mapConcurrently (storeDocIfNotExist storageConfig respStreamBody) msgAttachmentsList
  case errs of
    [] -> do
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
          logError $ "Error while getting relevant docs" <> showText err
          pure msgList_
        Right sysPrompt -> do
          let sysMsg = L.Message L.System sysPrompt L.defaultMessageData
          let userMsg = L.Message L.User lastUserMsg L.defaultMessageData
          logDebug $ "sysPrompt" <> L.content sysMsg
          pure $ NE.fromList $ NE.init msgList_ ++ [sysMsg, userMsg]
    lst -> do
      logDebug $ "Error while generating docs" <> showText lst
      pure msgList_

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
  StreamHandler OpenAIInternal.ChatCompletionChunk ->
  Text ->
  IO (Either LangchainError ())
runOpenRouterWithTools llm msgList sh tool = do
  let toolDefs = getOpenRouterToolDefinitions tool
      openAIParams = defaultOpenAIParams {OpenAILike.tools = Just toolDefs}

  -- First call to get tool calls
  eRes <- chat llm msgList (Just openAIParams)
  case eRes of
    Left err -> pure $ Left err
    Right response -> do
      case toolCalls (messageData response) of
        Nothing -> do
          -- No tool calls, proceed with normal streaming
          void $ stream llm msgList sh Nothing
          pure $ Right ()
        Just toolCallList -> do
          -- Execute tool calls and add results to message list
          toolResults <- mapConcurrently (executeOpenRouterToolCall tool) toolCallList
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
  StreamHandler Ollama.ChatResponse ->
  Text ->
  IO (Either LangchainError ())
runOllamaWithTools llm msgList sh tool = do
  let toolDefs = getToolDefinitions tool
      ollamaParams = defaultOllamaParams {LLMOllama.tools = Just toolDefs}
  -- First call to get tool calls
  eRes <- chat llm msgList (Just ollamaParams)
  case eRes of
    Left err -> pure $ Left err
    Right response -> do
      case toolCalls (messageData response) of
        Nothing -> do
          -- No tool calls, proceed with normal streaming
          print ("no tools were called" :: String)
          void $ stream llm msgList sh Nothing
          pure $ Right ()
        Just toolCallList -> do
          -- Execute tool calls and add results to message list
          toolResults <- mapConcurrently (executeToolCallFromResponse tool) toolCallList
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
    IO (Either LangchainError ())

  streamWithTools ::
    a ->
    NE.NonEmpty Langchain.Message ->
    StreamHandler Text ->
    Text ->
    IO (Either LangchainError ())

  generateNewConversationTitle ::
    a ->
    Text ->
    IO (Either LangchainError NewConversationTitle)
  summarizeOlderConversation :: a -> Text -> IO (Either LangchainError Text)

data AnyLLMProvider where
  AnyLLMProvider :: (LLMProvider a) => a -> AnyLLMProvider

streamWithProvider ::
  AnyLLMProvider ->
  NE.NonEmpty Langchain.Message ->
  StreamHandler Text ->
  Maybe Text ->
  IO (Either LangchainError ())
streamWithProvider (AnyLLMProvider provider) msgs sh Nothing =
  streamResponse provider msgs sh Nothing
streamWithProvider (AnyLLMProvider provider) msgs sh (Just tool) =
  streamWithTools provider msgs sh tool

toOllamaHandler :: StreamHandler Text -> StreamHandler Ollama.ChatResponse
toOllamaHandler sh =
  StreamHandler
    { onToken = onToken sh . Ollama.content . fromMaybe emptyMsg . Ollama.message
    , onComplete = onComplete sh
    }
  where
    emptyMsg = Ollama.assistantMessage ""

instance LLMProvider Ollama where
  streamResponse provider msgs sh _ =
    Langchain.stream provider msgs (toOllamaHandler sh) Nothing
  streamWithTools provider msgs sh = runOllamaWithTools provider msgs (toOllamaHandler sh)
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
  summarizeOlderConversation l oldConv = do
    let msgContent = summarizeConversationHistoryPrompt <> oldConv
    let msg = NE.fromList [Message OpenAILike.User msgContent defaultMessageData]
    fmap content <$> chat l msg Nothing

openAIChunkToText :: OpenAIInternal.ChatCompletionChunk -> T.Text
openAIChunkToText completionChunk = do
  fromMaybe ""
    . OpenAIInternal.contentForDelta
    . OpenAIInternal.chunkChoiceDelta
    . fromMaybe emptyChoice
    . listToMaybe
    $ OpenAIInternal.chunkChoices completionChunk
  where
    emptyChoice =
      OpenAIInternal.ChunkChoice
        ( OpenAIInternal.Delta
            { OpenAIInternal.contentForDelta = Nothing
            , OpenAIInternal.deltaRole = Nothing
            , OpenAIInternal.deltaToolCalls = Nothing
            , OpenAIInternal.deltaRefusal = Nothing
            , OpenAIInternal.deltaFunctionCall = Nothing
            }
        )
        1
        Nothing
        Nothing

toOpenAIStreamHandler ::
  StreamHandler Text -> StreamHandler OpenAIInternal.ChatCompletionChunk
toOpenAIStreamHandler sh =
  StreamHandler
    { onToken = onToken sh . openAIChunkToText
    , onComplete = onComplete sh
    }

instance LLMProvider OpenAICompatible where
  streamResponse provider msgs sh =
    Langchain.stream provider msgs (toOpenAIStreamHandler sh)
  streamWithTools provider msgs sh =
    runOpenRouterWithTools
      provider
      msgs
      (toOpenAIStreamHandler sh)
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
  summarizeOlderConversation l oldConv = do
    let msgContent = summarizeConversationHistoryPrompt <> oldConv
    let msg = NE.fromList [Message OpenAILike.User msgContent defaultMessageData]
    fmap content <$> chat l msg Nothing

instance LLMProvider Gemini where
  streamResponse provider msgs sh =
    Langchain.stream provider msgs (toOpenAIStreamHandler sh)
  streamWithTools provider msgs sh =
    runOpenRouterWithTools
      (geminiToOpenAICompatible provider)
      msgs
      (toOpenAIStreamHandler sh)
  generateNewConversationTitle =
    generateNewConversationTitle . geminiToOpenAICompatible
  summarizeOlderConversation l oldConv = do
    let msgContent = summarizeConversationHistoryPrompt <> oldConv
    let msg = NE.fromList [Message OpenAILike.User msgContent defaultMessageData]
    fmap content <$> chat l msg Nothing

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

summarizeConversationHistoryPrompt :: Text
summarizeConversationHistoryPrompt =
  T.unlines
    [ "You are given a list of older conversation, due to context length"
    , "We cannot pass these older messages to the LLM, your goal is to"
    , "Create a paragraph that summarizes the old messages in 5-6 lines."
    , "Only return the summary and nothing else!!!!"
    , "older converastion: "
    ]

summarizeConversationHistory ::
  LLMRespStreamBody ->
  [ChatMessageWithAttachments] ->
  AppM (Either LangchainError Message)
summarizeConversationHistory _ [] = pure $ Left $ fromString "No messages to summarize"
summarizeConversationHistory llmRespBody chatMsgAttLst = do
  let mbConvID = chatMessageConversationID . cm <$> listToMaybe chatMsgAttLst
  case mbConvID of
    Nothing -> pure $ Left $ fromString "No conversation ID found in messages"
    Just convID -> do
      let oldConvo = foldr combineContent "Older conversation context: \n" chatMsgAttLst
      eLLM <- mkLLMProvider llmRespBody
      case eLLM of
        Left e -> pure . Left . fromString $ show e
        Right anyLLMProvider -> getOrCreateConversationSummary anyLLMProvider convID (T.take 5000 oldConvo)
  where
    combineContent chatMsgAtt acc = do
      let c = cm chatMsgAtt
      let cont = chatMessageContent c
      if chatMessageRole c == MessageRoleUser
        then
          acc `T.append` "User: " <> cont <> "\n"
        else acc `T.append` "Assistant: " <> cont <> "\n"

getOrCreateConversationSummary ::
  AnyLLMProvider -> ConversationID -> Text -> AppM (Either LangchainError Message)
getOrCreateConversationSummary (AnyLLMProvider llmProvider) convId oldConvo = do
  mbSummary <- findSummaryByConvID convId
  case mbSummary of
    Just summary -> pure $ Right $ Message Langchain.System (oldConvSummarySummary summary) defaultMessageData
    Nothing -> do
      eSummaryText <- liftIO $ summarizeOlderConversation llmProvider oldConvo
      case eSummaryText of
        Left err -> pure $ Left err
        Right summaryText -> do
          let summaryMsg =
                Message
                  Langchain.System
                  summaryText
                  defaultMessageData
          void $
            addSummary $
              OldConvSummary
                { oldConvSummaryID = ()
                , oldConvSummaryConversationID = convId
                , oldConvSummarySummary = summaryText
                , oldConvSummaryCreatedAt = ()
                }
          pure $ Right summaryMsg
