{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module App.View.GenerateReply
  ( GenerateReplyView (..)
  , Action (Generate)
  ) where

import App.Common.Types
import App.Common.Utils
import App.Effects.StateStore
import App.View.Chat (renderMessageBubble)
import App.DB
import App.LLM
import Control.Concurrent (MVar, forkIO, modifyMVar_)
import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Langchain.LLM.Core (StreamHandler (..))
import Web.Hyperbole
import App.Common.MarkdownAtomic (parseView)

data GenerateReplyView = GenerateReplyView Int
  deriving (Generic, ViewId)

instance (IOE :> es, StateStoreEff :> es) => HyperView GenerateReplyView es where
  data Action GenerateReplyView
    = Generate Text (Maybe Text) (Maybe AvailableTool)
    | Stream
    deriving (Generic, ViewAction)

  update = \case
    Generate p mbFilePath mbTool -> do
      (GenerateReplyView chatId) <- viewId
      generateReply chatId p mbFilePath mbTool
    Stream -> do
      (GenerateReplyView chatId) <- viewId
      streamReply chatId

generateReply ::
  (StateStoreEff :> es, IOE :> es) =>
  Int ->
  Text ->
  Maybe Text ->
  Maybe AvailableTool ->
  Eff es (View GenerateReplyView ())
generateReply chatId p mbFilePath mbTool = do
  initializeStreaming chatId
  provider <- getProviderInfo
  startGeneration provider chatId p mbFilePath mbTool
  pure $ renderGeneratedReply "generating..."

streamReply ::
  (StateStoreEff :> es, IOE :> es, Hyperbole :> es) =>
  Int -> Eff es (View GenerateReplyView ())
streamReply chatId = do
  streamState <- getStreamState chatId
  case streamState of
    Just (Complete content) -> handleCompleteStream chatId content
    Just (InProgress content) -> pure $ renderGeneratedReply (parseView content)
    Nothing -> pure $ renderMessageBubble "Something went wrong" False

handleCompleteStream ::
  (IOE :> es, Hyperbole :> es) =>
  Int ->
  Text ->
  Eff es (View GenerateReplyView ())
handleCompleteStream chatId content = do
  eRes <- liftIO $ try $ withDatabase "chat.db" $ \conn -> do
    void $ addMessage conn Assistant content chatId Nothing
    getConversationMessages conn chatId
  case eRes of
    Left err -> do
      liftIO $ print (err :: SomeException)
      pure $ text $ T.pack $ show err
    Right messages ->
      if length messages > 2
        then pure $ renderMessageBubble (parseView content) False
        else redirect $ relUrl ("/chat/" <> T.pack (show chatId))

initializeStreaming :: (StateStoreEff :> es, IOE :> es) => Int -> Eff es ()
initializeStreaming chatId = do
  storeVar <- useState
  liftIO $ modifyMVar_ storeVar $ \s ->
    pure s {streamContent = HM.insert chatId (InProgress "") (streamContent s)}

startGeneration ::
  (IOE :> es, StateStoreEff :> es) =>
  Provider ->
  Int ->
  Text ->
  Maybe Text ->
  Maybe AvailableTool ->
  Eff es ()
startGeneration provider chatId p mbFilePath mbTool = do
  storeVar <- useState
  liftIO . void . forkIO . void $
    runPrompt
      provider
      (createStreamHandler storeVar chatId)
      chatId
      p
      mbFilePath
      mbTool

createStreamHandler :: MVar StateStore -> Int -> StreamHandler
createStreamHandler storeVar chatId = StreamHandler onTokenFn onCompleteFn
  where
    onTokenFn tok = do
      modifyMVar_ storeVar $ \s ->
        let newText = case HM.lookup chatId (streamContent s) of
              Just (InProgress txt) -> txt <> tok
              _ -> tok
         in pure
              s
                { streamContent =
                    HM.insert
                      chatId
                      (InProgress newText)
                      (streamContent s)
                }
    onCompleteFn = do
      modifyMVar_ storeVar $ \s ->
        case HM.lookup chatId (streamContent s) of
          Just (InProgress txt) ->
            pure s {streamContent = HM.insert chatId (Complete txt) (streamContent s)}
          _ -> pure s
      putStrLn "Streaming complete"

renderGeneratedReply :: View GenerateReplyView () -> View GenerateReplyView ()
renderGeneratedReply content =
  el @ onLoad Stream 300 $ renderMessageBubble content False
