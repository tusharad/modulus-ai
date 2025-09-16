{-# LANGUAGE ScopedTypeVariables #-}

module Modulus.Common.Utils
  ( listAvailableOllamaModels
  , takeRecentMessages
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Modulus.BE.DB.Internal.Model
import qualified Ollama

listAvailableOllamaModels :: IO [Text]
listAvailableOllamaModels = do
  eVersion <- Ollama.getVersion
  case eVersion of
    Left _ -> do
      putStrLn "Ollama not seems to be installed."
      pure []
    Right (Ollama.Version v) -> do
      putStrLn $ "Ollama version: " <> T.unpack v
      eModelInfo <- Ollama.list Nothing
      case eModelInfo of
        Left err -> print err >> pure []
        Right (Ollama.Models modelInfoList) -> do
          let modelNames = map Ollama.name modelInfoList
          pure modelNames

takeRecentMessages ::
  Int ->
  NE.NonEmpty ChatMessageWithAttachments ->
  (NE.NonEmpty ChatMessageWithAttachments, [ChatMessageWithAttachments])
takeRecentMessages charLimit msgs_ = do
  let (lastMsg :| remainingMsgs) = NE.reverse msgs_
  let go _ kept [] = (kept, [])
      go accLen kept (m : ms) = do
        let newLen = accLen + T.length (chatMessageContent $ cm m)
        if newLen >= charLimit
          then (kept, reverse (m : ms))
          else go newLen (m NE.<| kept) ms
   in go 0 (lastMsg :| []) remainingMsgs
