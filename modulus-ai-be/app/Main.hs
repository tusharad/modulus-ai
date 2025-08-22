module Main (main) where

import Modulus.BE.Api.Server
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.Monad.Utils (mkAppConfigFromEnv)
import Modulus.Common.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Orville.PostgreSQL (runOrvilleWithState)

myCorsPolicy :: CorsResourcePolicy
myCorsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

myCorsMiddleware :: Middleware
myCorsMiddleware = cors (const $ Just myCorsPolicy)

main :: IO ()
main = do
  eConf <- mkAppConfigFromEnv
  case eConf of
    Left err -> print err
    Right conf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState conf) autoMigrateQ
      putStrLn $ "Running application on port " <> show (configPort conf)
      run (configPort conf) $
        staticPolicy (addBase "public") $
          myCorsMiddleware $
            appToServer conf
