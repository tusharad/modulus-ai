module Main (main) where

import Modulus.BE.Api.Server
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Monad.AppM (AppConfig (..))
import Modulus.BE.Monad.Utils (mkAppConfigFromEnv)
import Network.Wai.Handler.Warp (run)
import Orville.PostgreSQL (runOrvilleWithState)

main :: IO ()
main = do
  eConf <- mkAppConfigFromEnv
  case eConf of
    Left err -> print err
    Right conf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState conf) autoMigrateQ
      putStrLn $ "Running application on port " <> show (configPort conf)
      run (configPort conf) (appToServer conf)
