module Main (main) where

import Modulus.BE.Api.Server
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Monad.Utils (mkAppConfigFromEnv)
import qualified Modulus.FE.Core as FE
import Modulus.FE.Effects.StateStore (StateStoreMap, initStateStoreMap)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Orville.PostgreSQL (runOrvilleWithState)
import Modulus.Common.Types (AppConfig (..))

main :: IO ()
main = do
  eConf <- mkAppConfigFromEnv
  case eConf of
    Left err -> print err
    Right conf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState conf) autoMigrateQ
      stateMap <- initStateStoreMap
      putStrLn $ "Running application on port " <> show (configPort conf)
      run (configPort conf) $
        staticPolicy (addBase "public") $
          mainApp stateMap conf (appToServer conf)

mainApp :: StateStoreMap -> AppConfig -> Application -> Application
mainApp stateMap appCfg servantAppInst req respond = do
  case pathInfo req of
    ("api" : _) -> servantAppInst req respond
    _ -> FE.app stateMap appCfg req respond
