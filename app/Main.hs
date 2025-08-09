module Main (main) where

import Data.Either (fromRight)
import Modulus.BE.Api.Server
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Monad.Utils (mkAppConfigFromEnv)
import Modulus.Common.Openrouter
import Modulus.Common.Types
import Modulus.Common.Utils (listAvailableOllamaModels)
import qualified Modulus.FE.Core as FE
import Modulus.FE.Effects.StateStore (StateStoreMap, initStateStoreMap)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Orville.PostgreSQL (runOrvilleWithState)

main :: IO ()
main = do
  eConf <- mkAppConfigFromEnv
  case eConf of
    Left err -> print err
    Right conf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState conf) autoMigrateQ
      stateMap <- initStateStoreMap
      ollamaModelList <- listAvailableOllamaModels
      openRouterModels <-
        map modelId . take 5 . fromRight []
          <$> getOpenRouterModelList
      let stData = StateStoreData ollamaModelList openRouterModels 
      putStrLn $ "Running application on port " <> show (configPort conf)
      run (configPort conf) $
        staticPolicy (addBase "public") $
          mainApp stateMap stData conf (appToServer conf)

mainApp ::
  StateStoreMap ->
  StateStoreData ->
  AppConfig ->
  Application ->
  Application
mainApp stateMap stData appCfg servantAppInst req respond = do
  case pathInfo req of
    ("api" : _) -> servantAppInst req respond
    _ -> FE.app stateMap stData appCfg req respond
