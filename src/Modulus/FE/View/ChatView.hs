module Modulus.FE.View.ChatView
  ( loadChatView
  , ChatView (..)
  , Action (..)
  , GenerateReplyView (..)
  ) where

import Control.Concurrent (MVar, forkIO, modifyMVar_)
import Control.Monad (forM_, void)
import qualified Data.Map.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import Effectful (IOE, MonadIO (liftIO))
import Modulus.BE.Api.Types
import Modulus.BE.DB.Internal.Model
import Modulus.BE.Handler.Conversations
import Modulus.BE.Log (logDebug)
import Modulus.Common.Utils (runBE, runBEAuth)
import Modulus.FE.Effects.AppConfig (AppConfigEff)
import Modulus.FE.Effects.StateStore
import Modulus.FE.MarkdownAtomic (parseView)
import Modulus.FE.Utils
import Modulus.FE.View.ModelProviderView
import Modulus.FE.View.SidebarView (Action (LoadSidebar), SidebarView (SidebarView))
import qualified Servant.Types.SourceT as S
import Web.Atomic.CSS
import Web.Hyperbole

data ChatView = ChatView Text
  deriving (Generic, ViewId)

instance
  (IOE :> es, AppConfigEff :> es, StateStoreEff :> es) =>
  HyperView ChatView es
  where
  data Action ChatView = LoadChatView Text | ProcessPrompt
    deriving (Generic, ViewAction)

  type Require ChatView = '[SidebarView, GenerateReplyView]

  update ProcessPrompt = do
    st <- getState
    let currPrompt = currentPrompt st
    void . runBE $ logDebug $ "Processing prompt : " <> currPrompt
    if T.null currPrompt
      then pure $ errorView "prompt appears to be empty"
      else do
        (ChatView convID) <- viewId
        convPublicID <- resolveOrCreateConvoID convID currPrompt
        addUserPrompt convPublicID currPrompt
  update (LoadChatView publicConvID) = do
    case UUID.fromText publicConvID of
      Nothing -> redirect chatUrlNew
      Just convUUID -> do
        eMsgList <-
          runBEAuth (`getConversationMessagesHandler` ConversationPublicID convUUID)
        case eMsgList of
          Left err -> do
            _ <-
              runBE . logDebug $
                "Error while fetching messages: " <> T.pack (show err)
            pure $ errorView (T.pack (show err))
          Right msgList -> pure $ chatView msgList

addUserPrompt ::
  ( IOE :> es
  , AppConfigEff :> es
  , Hyperbole :> es
  , StateStoreEff :> es
  ) =>
  ConversationPublicID ->
  Text ->
  Eff es (View ChatView ())
addUserPrompt convID currPrompt = do
  eRes <-
    runBEAuth
      ( \auth ->
          addConversationMessageHandler
            auth
            convID
            (AddMessageRequest currPrompt "user" Nothing Nothing)
      )
  case eRes of
    Left err -> pure $ errorView $ T.pack (show err)
    Right _ -> do
      modifyState $ \st_ -> st_ {currentPrompt = mempty}
      eMsgList <- runBEAuth (\auth -> getConversationMessagesHandler auth convID)
      case eMsgList of
        Left err -> pure $ errorView $ T.pack (show err)
        Right msgList -> do
          void . runBE $ logDebug "Rerendered msg list: "
          pure $ renderGeneratingReplyView convID msgList

resolveOrCreateConvoID ::
  ( IOE :> es
  , AppConfigEff :> es
  , Hyperbole :> es
  ) =>
  Text ->
  Text ->
  Eff es ConversationPublicID
resolveOrCreateConvoID convID currPrompt = do
  if convID == "1"
    then do
      let addConvReq = AddConversationRequest (T.take 20 currPrompt)
      eConvPublicID <- runBEAuth (`addConversationHandler` addConvReq)
      -- TODO: Fork a process to generate and update the conversation title
      case eConvPublicID of
        Left _ -> redirect chatUrlNew -- TODO: should render error view
        Right convPublicID -> pure convPublicID
    else do
      case UUID.fromText convID of
        Nothing -> redirect chatUrlNew -- TODO: should render error view
        Just x -> pure $ ConversationPublicID x

errorView :: Text -> View c ()
errorView x = el ~ cls "message ai-message" $ text x

utcToText :: UTCTime -> Text
utcToText = T.pack . formatTime defaultTimeLocale "%F %T"

renderChatMsg :: ChatMessageRead -> View c ()
renderChatMsg chatMsg = do
  let css_ =
        if chatMessageRole chatMsg == MessageRoleUser
          then
            "message user-message"
          else "message ai-message"
  el ~ cls css_ $ text (chatMessageContent chatMsg)
  el ~ cls "message-meta" $ text (utcToText (chatMessageCreatedAt chatMsg))

renderGeneratingReplyView :: ConversationPublicID -> [ChatMessageRead] -> View ChatView ()
renderGeneratingReplyView (ConversationPublicID convID) chatMsgList = do
  let c = UUID.toText convID
  target (SidebarView 0) $ el @ onLoad LoadSidebar 300 $ none
  el ~ cls "chat-window" $ do
    forM_ chatMsgList $ \chatMsg -> renderChatMsg chatMsg
    hyper (GenerateReplyView c) $ el ~ cls "message ai-message" $ spinner
    target (GenerateReplyView c) $ el @ onLoad Generate 300 $ none

chatView :: [ChatMessageRead] -> View ChatView ()
chatView chatMsgList = do
  el ~ cls "chat-window" $ do
    forM_ chatMsgList $ \chatMsg -> renderChatMsg chatMsg

spinner :: View c ()
spinner =
  el ~ cls "text-center py-5" $ do
    el ~ cls "spinner-border text-light" @ att "role" "status" $
      tag "span" ~ cls "visually-hidden" $
        "loading..."

loadChatView :: Maybe Text -> View ChatView ()
loadChatView Nothing = el ~ cls "chat-window" $ none -- TODO: For new chat, add some Hi message
loadChatView (Just convId) = do
  el @ onLoad (LoadChatView convId) 200 $
    el ~ cls "text-center py-5" $ do
      el ~ cls "spinner-border text-light" @ att "role" "status" $
        tag "span" ~ cls "visually-hidden" $
          "loading..."

----
data GenerateReplyView = GenerateReplyView Text
  deriving (Generic, ViewId)

instance
  ( IOE :> es
  , StateStoreEff :> es
  , AppConfigEff :> es
  , Hyperbole :> es
  ) =>
  HyperView GenerateReplyView es
  where
  data Action GenerateReplyView
    = Generate
    | Stream
    deriving (Generic, ViewAction)

  update = \case
    Generate -> do
      (GenerateReplyView convID) <- viewId
      generateReply convID
    Stream -> do
      (GenerateReplyView convID) <- viewId
      streamReply convID

streamReply ::
  ( StateStoreEff :> es
  , IOE :> es
  , Hyperbole :> es
  , AppConfigEff :> es
  ) =>
  Text ->
  Eff es (View GenerateReplyView ())
streamReply convID = do
  streamState <- getStreamState convID
  case streamState of
    Just (Complete content) -> do
      _ <- runBE $ logDebug $ "Streaming complte "
      handleCompleteStream convID content
    Just (InProgress content) -> pure $ renderStreamingReply (Just (parseView content))
    Nothing -> pure $ el ~ cls "message ai-message" $ "Something went wrong"

handleCompleteStream ::
  ( IOE :> es
  , Hyperbole :> es
  , AppConfigEff :> es
  , StateStoreEff :> es
  ) =>
  Text ->
  Text ->
  Eff es (View GenerateReplyView ())
handleCompleteStream convID content = do
  case UUID.fromText convID of
    Nothing -> pure $ errorView "UUID converstion failed"
    Just publicConvID -> do
      provider <- getProviderInfo
      let (model, p, _) = case provider of
            OllamaProvider m -> (m, "ollama", Nothing)
            OpenRouterProvider m api -> (m, "openrouter", Just api)
      let req =
            AddMessageRequest
              { messageContent = content
              , addMessageRole = "assistant"
              , addMessageProvider = Just p
              , addMessageModel = Just model
              }
      eRes <-
        runBEAuth
          ( \auth ->
              addConversationMessageHandler
                auth
                (ConversationPublicID publicConvID)
                req
          )
      case eRes of
        Left err ->
          pure $
            errorView
              ("something went wrong during streaming " <> T.pack (show err))
        Right _ -> do
          eMsgList <-
            runBEAuth
              (`getConversationMessagesHandler` ConversationPublicID publicConvID)
          case eMsgList of
            Left err -> do
              _ <- runBE $ logDebug $ "Error while fetching messages: " <> T.pack (show err)
              pure $ errorView (T.pack (show err))
            Right msgList ->
              if length msgList > 2
                then
                  pure $ el ~ cls "message ai-message" $ parseView content
                else redirect $ chatUrl (ConversationPublicID publicConvID)

renderStreamingReply :: Maybe (View GenerateReplyView ()) -> View GenerateReplyView ()
renderStreamingReply mbContent =
  el @ onLoad Stream 200 $
    el ~ cls "message ai-message" $
      fromMaybe spinner mbContent

generateReply ::
  ( StateStoreEff :> es
  , IOE :> es
  , AppConfigEff :> es
  , Hyperbole :> es
  ) =>
  Text ->
  Eff es (View GenerateReplyView ())
generateReply convID = do
  provider <- getProviderInfo
  let (model, p, mbApiKey) = case provider of
        OllamaProvider m -> (m, "ollama", Nothing)
        OpenRouterProvider m api -> (m, "openrouter", Just api)
  let reqBody =
        LLMRespStreamBody
          { modelUsed = model
          , provider = p
          , apiKey = mbApiKey
          }
  void . runBE $ logDebug $ "In generateReply function: " <> model <> p
  startGeneration convID reqBody
  pure $ renderStreamingReply Nothing

startGeneration ::
  ( IOE :> es
  , StateStoreEff :> es
  , AppConfigEff :> es
  , Hyperbole :> es
  ) =>
  Text ->
  LLMRespStreamBody ->
  Eff es ()
startGeneration convID body = do
  modifyState
    ( \s ->
        s
          { streamContent =
              HM.insert convID (InProgress "") (streamContent s)
          }
    )
  storeVar <- useState
  case UUID.fromText convID of
    Nothing -> pure ()
    Just publicConvID -> do
      eSrc <- runBEAuth $ \auth ->
        getLLMRespStreamHandler
          auth
          (ConversationPublicID publicConvID)
          body
      case eSrc of
        Left err -> do
          _ <- runBE $ logDebug $ "Error while calling stream: " <> T.pack (show err)
          pure ()
        Right s -> do
          liftIO . void . forkIO $ S.unSourceT s (processChunks storeVar convID)

processChunks :: MVar StateStore -> Text -> S.StepT IO LLMRespStream -> IO ()
processChunks storeVar convID (S.Yield chunk nextStep) = do
  appendChunk storeVar convID chunk
  processChunks storeVar convID nextStep
processChunks storeVar convID S.Stop = do
  putStrLn "streaming complete"
  markComplete storeVar convID
processChunks _ _ (S.Error err) = do
  putStrLn $ "Error in servant stream " <> show err
processChunks storeVar convID (S.Skip s) = do
  processChunks storeVar convID s
processChunks storeVar convID (S.Effect ms) = do
  ms >>= processChunks storeVar convID

markComplete :: MVar StateStore -> Text -> IO ()
markComplete storeVar convID =
  modifyMVar_ storeVar $ \s ->
    case HM.lookup convID (streamContent s) of
      Just (InProgress acc) ->
        pure
          s
            { streamContent = HM.insert convID (Complete acc) (streamContent s)
            }
      _ -> pure s

appendChunk :: MVar StateStore -> Text -> LLMRespStream -> IO ()
appendChunk storeVar convID (LLMRespStream txt _ _) =
  modifyMVar_ storeVar $ \s ->
    let updated = case HM.lookup convID (streamContent s) of
          Just (InProgress acc) -> InProgress (acc <> txt)
          _ -> InProgress txt
     in pure s {streamContent = HM.insert convID updated (streamContent s)}
