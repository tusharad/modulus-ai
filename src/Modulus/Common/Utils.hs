module Modulus.Common.Utils (
    runBE
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Modulus.FE.Effects.AppConfig (getAppCfg, AppConfigEff)
import Modulus.BE.Monad.AppM (runAppM, AppM)
import Web.Hyperbole
import Modulus.BE.Monad.Error (AppError)
import Effectful (IOE)

runBE :: (IOE :> es, AppConfigEff :> es) => AppM a -> Eff es (Either AppError a)
runBE action = do 
  cfg <- getAppCfg
  liftIO $ runAppM cfg action
