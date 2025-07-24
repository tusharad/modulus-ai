{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module App.Core (runApp) where

import App.Common.Types
import App.Common.Utils (listAvailableOllamaModels)
import App.Effects.StateStore
import App.OpenRouter.Models (OpenRouterModel (..), getOpenRouterModelList)
import qualified App.Page.Chat as Chat
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Effectful
import Network.HTTP.Types (status400)
import Network.Wai (pathInfo)
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Web.Hyperbole
import Web.Scotty (ScottyM, files, get, post, scottyApp, status)
import qualified Web.Scotty as Scotty

toDocument :: BL.ByteString -> BL.ByteString
toDocument cnt =
  [i|<html>
      <head>
        <title>AI Chatbot</title>
        <meta httpEquiv="Content-Type" content="text/html" charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <script type="text/javascript">#{scriptEmbed}</script>
        <style type="text/css">#{cssResetEmbed}</style>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.7/dist/css/bootstrap.min.css" 
        rel="stylesheet" 
        integrity="sha384-LN+7fdVzj6u52u30Kp6M/trliBMCMKTyK833zpbD+pXdCLuTusPj697FH4R/5mcr" 
        crossorigin="anonymous">
        <link rel="stylesheet" 
        href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.2/css/all.min.css" 
        integrity="sha512-Evv84Mr4kqVGRNSgIGL/F/aIDqQb7xQ2vcrdIwxfjThSH8CSR7PBEakCr51Ck+w+/U6swU2Im1vVX0SVk9ABhg==" 
        crossorigin="anonymous" referrerpolicy="no-referrer" />
      </head>
      <body data-bs-theme="light">#{cnt}</body>
      <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.7/dist/js/bootstrap.bundle.min.js" 
      integrity="sha384-ndDqU0Gzau9qJ1lfW4pNLlhNTkCfHzAVBReH9diLvGRem5+R9g2FzA8ZGN954O5Q" 
      crossorigin="anonymous"></script>
    </html>|]

router ::
  (IOE :> es, Hyperbole :> es, StateStoreEff :> es) =>
  AppRoute -> Eff es Response
router r = do
  case r of
    Main -> redirect (routeUri $ Chat Nothing)
    Chat mbChatId -> runPage $ Chat.page (fromMaybe 0 mbChatId)

app :: StateStoreMap -> [Text] -> [Text] -> Application
app stateMap ollamaModelList orModelList =
  liveApp
    toDocument
    (runM . routeRequest $ router)
  where
    runM :: (IOE :> es, Hyperbole :> es) => Eff (StateStoreEff : es) a -> Eff es a
    runM = runStateStoreIO stateMap ollamaModelList orModelList

mainApp :: StateStoreMap -> [Text] -> [Text] -> Application -> Application
mainApp stateMap ollamaModelList orModelList scottyAppInst req respond = do
  case pathInfo req of
    ("api" : _) -> scottyAppInst req respond
    _ -> app stateMap ollamaModelList orModelList req respond

scottySubApp :: ScottyM ()
scottySubApp = do
  get "/hello" $
    Scotty.text "Hello from Scotty!"
  post "/api/upload_attachment" $ do
    fs <- files
    case lookup "file" fs of
      Just f -> do
        let path = "uploads/" <> BC.unpack (fileName f)
        liftIO $ BL.writeFile path (fileContent f)
        Scotty.text (TL.pack path)
      Nothing -> status status400

runApp :: IO ()
runApp = do
  let port = 3000
  stateMap <- initStateStoreMap
  ollamaModelList <- listAvailableOllamaModels
  openRouterModels <-
    (\x -> map modelId $ take 5 $ fromRight [] x)
      <$> getOpenRouterModelList
  putStrLn $ "Starting Examples on http://localhost:" <> show port
  scottyAppInstance <- scottyApp scottySubApp
  run port $
    staticPolicy (addBase "public/static") $
      mainApp stateMap ollamaModelList openRouterModels scottyAppInstance
