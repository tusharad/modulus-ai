-- LLM related functions shall be here
module Modulus.BE.LLM
  ( attachDocumentRAG
  , runOpenRouterWithTools
  , runOllamaWithTools
  ) where

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class
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
import Langchain.DocumentLoader.Core (BaseLoader (load), Document (..))
import Langchain.DocumentLoader.FileLoader (FileLoader (FileLoader))
import Langchain.DocumentLoader.PdfLoader (PdfLoader (PdfLoader))
import Langchain.Embeddings.Ollama (OllamaEmbeddings (OllamaEmbeddings))
import qualified Langchain.LLM.Core as L
import qualified Langchain.LLM.Core as Langchain
import qualified Langchain.LLM.Internal.OpenAI as OpenAIInternal
import Langchain.LLM.Ollama as LLMOllama
import Langchain.LLM.OpenAICompatible as OpenAILike hiding (metadata)
import Langchain.PromptTemplate (PromptTemplate (PromptTemplate), renderPrompt)
import Langchain.Retriever.Core
  ( Retriever (_get_relevant_documents)
  , VectorStoreRetriever (VectorStoreRetriever)
  )
import Langchain.Tool.Core (Tool (runTool))
import Langchain.Tool.WebScraper (WebScraper (WebScraper))
import Langchain.Tool.WikipediaTool (defaultWikipediaTool)
import Langchain.VectorStore.InMemory (fromDocuments)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.Log (logDebug, logError)
import Modulus.BE.Monad.AppM (AppM)
import Modulus.BE.Monad.Storage
import System.FilePath
import UnliftIO.Directory (removeFile)

-- | Create tool message from tool result
createToolMessage :: (String, Text) -> Message
createToolMessage (functionName, result) =
  Message
    System
    ("Tool (" <> T.pack functionName <> ") result: " <> result)
    defaultMessageData

isBucket :: StorageConfig -> Bool
isBucket (StorageBucket _) = True
isBucket _ = False

attachDocumentRAG ::
  [MessageAttachmentRead] ->
  NE.NonEmpty L.Message ->
  AppM (NE.NonEmpty L.Message)
attachDocumentRAG msgAttachmentsList msgList_ = do
  storageConfig <- mkStorageFromEnv
  eVecStore <- runExceptT $ do
    docs <-
      mconcat
        <$> mapM
          ( \MessageAttachment {..} -> do
              let attName = T.unpack messageAttachmentFileName
              fPath <-
                ExceptT $
                  loadFile storageConfig attName
              if ".pdf" == takeExtension fPath
                then do
                  let sourcePdf = PdfLoader fPath
                  eRes <- ExceptT $ liftIO $ load sourcePdf
                  when (isBucket storageConfig) (removeFile fPath)
                  pure eRes
                else do
                  let sourcePdf = FileLoader fPath
                  eRes <- ExceptT $ liftIO $ load sourcePdf
                  when (isBucket storageConfig) (removeFile fPath)
                  pure eRes
          )
          msgAttachmentsList
    let ollamaEmbeddings =
          OllamaEmbeddings
            "nomic-embed-text:latest"
            Nothing
            Nothing
            Nothing
    ExceptT $ liftIO $ fromDocuments ollamaEmbeddings docs
  case eVecStore of
    Left err -> do
      logError $ "Error loading documents: " <> T.pack err
      pure msgList_
    Right vectorStore -> do
      let retriever = VectorStoreRetriever vectorStore
      let docToText =
            mconcat
              . map
                (\doc -> pageContent doc <> T.pack (show $ metadata doc))
      let promptTemplate = PromptTemplate systemTemplate
      let lastUserMsg = Langchain.content $ NE.last msgList_
      eSysPrompt <- runExceptT $ do
        relevantDocs <-
          ExceptT $
            liftIO $
              _get_relevant_documents retriever lastUserMsg
        let context = docToText relevantDocs
        ExceptT . pure $
          renderPrompt promptTemplate (HM.fromList [("context", context)])
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
    , "If you don't know the answer, just say that you don't know, don't try to make up an answer."
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
