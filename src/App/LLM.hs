module App.LLM
  ( runPrompt
  , generateTitle
  ) where

import App.Common.Types
  ( AvailableTool (..)
  , Provider (..)
  )
import qualified App.DB as DB
import Control.Monad (void)
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as HM
import Data.Ollama.Chat
  ( FunctionDef (..)
  , FunctionParameters (..)
  , InputTool (..)
  )
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.FileLoader
import Langchain.Embeddings.Core (Embeddings)
import Langchain.Embeddings.Ollama
import qualified Langchain.LLM.Internal.OpenAI as OpenAIInternal
import Langchain.LLM.Ollama as LLMOllama
import Langchain.LLM.OpenAICompatible as OpenAILike hiding (metadata)
import Langchain.PromptTemplate
import Langchain.Retriever.Core
import Langchain.Tool.Core (Tool (runTool))
import Langchain.Tool.WebScraper (WebScraper (WebScraper))
import Langchain.Tool.WikipediaTool (defaultWikipediaTool)
import Langchain.VectorStore.InMemory

toLLMMessage :: DB.ChatMessage -> Message
toLLMMessage DB.ChatMessage {..} = Message (toRole msgRole) msgContent defaultMessageData
  where
    toRole :: DB.CRole -> Role
    toRole = \case
      DB.User -> User
      DB.Assistant -> Assistant
      DB.System -> System

runPrompt ::
  Provider ->
  StreamHandler ->
  Int ->
  Text ->
  Maybe Text ->
  Maybe AvailableTool ->
  IO (Either String ())
runPrompt providerInfo sh chatId prompt mbFilePath mbTool = do
  contextMessages <- DB.withDatabase "chat.db" $ \conn ->
    reverse . drop 1 . take 10 . reverse <$> DB.getConversationMessages conn chatId
  -- \^ taking last 10 messages, dropping the last one since it's going to be prompt
  let chatHistory = map toLLMMessage contextMessages
  preparedMessage <- prepareMessages prompt mbFilePath
  let msgList = case chatHistory of
        [] -> preparedMessage
        _ -> NE.fromList chatHistory `NE.append` preparedMessage
  result <- selectAndRunProvider providerInfo msgList sh mbTool
  case result of
    Left err -> do
      onToken sh ("[ERROR] " <> (T.pack err))
      pure $ Left err
    Right _ -> pure $ Right ()

-- | Prepare message list based on whether file attachment is provided
prepareMessages :: Text -> Maybe Text -> IO (NE.NonEmpty Message)
prepareMessages prompt = \case
  Nothing -> pure $ NE.singleton $ Message User prompt defaultMessageData
  Just fp -> getPromptWithAttachmentContext prompt fp

-- | Select and run the appropriate provider based on configuration
selectAndRunProvider ::
  Provider ->
  NE.NonEmpty Message ->
  StreamHandler ->
  Maybe AvailableTool ->
  IO (Either String ())
selectAndRunProvider provider msgList sh mbTool =
  case provider of
    OllamaProvider modelName -> runOllama modelName msgList sh mbTool
    OpenRouterProvider _ "" -> pure $ Left "Missing api key for openrouter"
    OpenRouterProvider modelName apiKey -> runOpenRouter modelName apiKey msgList sh mbTool

-- | Run Ollama provider
runOllama ::
  Text ->
  NE.NonEmpty Message ->
  StreamHandler ->
  Maybe AvailableTool ->
  IO (Either String ())
runOllama model msgList sh mbTool = do
  let llm = Ollama model []
  case mbTool of
    Just tool -> runOllamaWithTools llm msgList sh tool
    Nothing -> stream llm msgList sh Nothing

runOllamaWithTools ::
  Ollama ->
  NE.NonEmpty Message ->
  StreamHandler ->
  AvailableTool ->
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

getToolDefinitions :: AvailableTool -> [InputTool]
getToolDefinitions = \case
  Web -> [ollamaWebScraperTool]
  Wiki -> [wikiSearchTool]

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

executeToolCallFromResponse :: AvailableTool -> ToolCall -> IO (String, Text)
executeToolCallFromResponse tool (ToolCall _ _ toolFunction) = do
  let functionName = toolFunctionName toolFunction
      arguments = toolFunctionArguments toolFunction

  case tool of
    Web -> do
      case HM.lookup "url" arguments of
        Just (String url) -> do
          result <- runTool WebScraper url
          case result of
            Left err -> pure (T.unpack functionName, T.pack err)
            Right content -> pure (T.unpack functionName, content)
        _ -> pure (T.unpack functionName, "Error: url parameter not found")
    Wiki -> do
      case HM.lookup "query" arguments of
        Just (String query) -> do
          result <- runTool defaultWikipediaTool query
          pure (T.unpack functionName, result)
        _ -> pure (T.unpack functionName, "Error: query parameter not found")

-- | Create tool message from tool result
createToolMessage :: (String, Text) -> Message
createToolMessage (functionName, result) =
  Message
    System
    ("Tool (" <> T.pack functionName <> ") result: " <> result)
    defaultMessageData

-- | Run OpenRouter provider
runOpenRouter ::
  Text ->
  Text ->
  NE.NonEmpty Message ->
  StreamHandler ->
  Maybe AvailableTool ->
  IO (Either String ())
runOpenRouter model apiKey msgList sh mbTool = do
  let llm = mkOpenRouter model [] Nothing apiKey
  case mbTool of
    Just tool -> runOpenRouterWithTools llm msgList sh tool
    Nothing -> stream llm msgList sh Nothing

runOpenRouterWithTools ::
  OpenAICompatible ->
  NE.NonEmpty Message ->
  StreamHandler ->
  AvailableTool ->
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

getOpenRouterToolDefinitions :: AvailableTool -> [OpenAIInternal.InputTool]
getOpenRouterToolDefinitions = \case
  Web -> [openRouterWebScraperTool]
  Wiki -> [openRouterWikiSearchTool]

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

executeOpenRouterToolCall :: AvailableTool -> ToolCall -> IO (String, Text)
executeOpenRouterToolCall tool (ToolCall _ _ toolFunction) = do
  let functionName = toolFunctionName toolFunction
      arguments = toolFunctionArguments toolFunction
  case tool of
    Web -> do
      case HM.lookup "url" arguments of
        Just (String url) -> do
          result <- runTool WebScraper url
          case result of
            Left err -> pure (T.unpack functionName, T.pack err)
            Right content -> pure (T.unpack functionName, content)
        _ -> pure (T.unpack functionName, "Error: url parameter not found")
    Wiki -> do
      case HM.lookup "query" arguments of
        Just (String query) -> do
          result <- runTool defaultWikipediaTool query
          pure (T.unpack functionName, result)
        _ -> pure (T.unpack functionName, "Error: query parameter not found")

generateTitle :: Text -> IO (Either String Text)
generateTitle userQuery = do
  let llm = Ollama "gemma3" []
      prompt = buildTitlePrompt userQuery
  generate llm prompt Nothing
  where
    buildTitlePrompt :: Text -> Text
    buildTitlePrompt query =
      T.unlines
        [ "For the below user question, generate a one line title for chatbot history."
        , "The title should summarize the user query."
        , "only and only the title, it should be only of one line"
        , "user query:"
        , query
        ]

systemTemplate :: Text
systemTemplate =
  T.unlines
    [ "Use the following pieces of context to answer the user's question."
    , "If you don't know the answer, just say that you don't know, don't try to make up an answer."
    , "ALWAYS return a \"SOURCES\" part in your answer."
    , "The \"SOURCES\" part should be a reference to the source of the document from which you got your answer."
    , "Example response:"
    , "```"
    , "The answer is foo."
    , "SOURCES: xyz"
    , "```"
    , "{context}"
    ]

getPromptWithAttachmentContext :: Text -> Text -> IO (NE.NonEmpty Message)
getPromptWithAttachmentContext prompt fp_ = do
  let fp = T.unpack fp_
      sourceDoc = FileLoader fp
      ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing
      userMsg = Message User prompt defaultMessageData

  result <- loadAndCreateVectorStore sourceDoc ollamaEmbeddings
  case result of
    Left err -> do
      putStrLn $ "Error loading documents: " <> err
      pure $ NE.singleton userMsg
    Right vectorStore -> do
      putStrLn "Document loaded.\n"
      createMessagesWithContext vectorStore prompt userMsg

-- | Load documents and create vector store
loadAndCreateVectorStore ::
  (Embeddings m) =>
  FileLoader ->
  m ->
  IO (Either String (InMemory m))
loadAndCreateVectorStore sourceDoc ollamaEmbeddings = runExceptT $ do
  docs <- ExceptT $ load sourceDoc
  ExceptT $ fromDocuments ollamaEmbeddings docs

-- | Create messages with RAG context
createMessagesWithContext ::
  Embeddings m =>
  (InMemory m) ->
  Text ->
  Message ->
  IO (NE.NonEmpty Message)
createMessagesWithContext vectorStore prompt userMsg = do
  let retriever = VectorStoreRetriever vectorStore
      promptTemplate = PromptTemplate systemTemplate

  result <- runExceptT $ do
    relevantDocs <- ExceptT $ _get_relevant_documents retriever prompt
    let context = documentsToText relevantDocs
    sysPrompt <-
      ExceptT . pure $
        renderPrompt promptTemplate (HM.fromList [("context", context)])
    let sysMsg = Message System sysPrompt defaultMessageData
    pure $ sysMsg NE.<| NE.singleton userMsg

  case result of
    Left err -> do
      putStrLn err
      pure $ NE.singleton userMsg
    Right messages -> pure messages

documentsToText :: [Document] -> Text
documentsToText = mconcat . map (\doc -> pageContent doc <> T.pack (show $ metadata doc))
