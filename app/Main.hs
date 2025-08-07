module Main (main) where

import Modulus.BE.Api.Server
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Monad.AppM (AppConfig (..))
import Modulus.BE.Monad.Utils (mkAppConfigFromEnv)
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
      putStrLn $ "Running application on port " <> show (configPort conf)
      run (configPort conf) $
        staticPolicy (addBase "public/static") $
          mainApp stateMap (appToServer conf)

mainApp :: StateStoreMap -> Application -> Application
mainApp stateMap servantAppInst req respond = do
  case pathInfo req of
    ("api" : _) -> servantAppInst req respond
    _ -> FE.app stateMap req respond
