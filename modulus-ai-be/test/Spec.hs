import qualified Conversation
import qualified DB
import Modulus.BE.Monad.Utils
import Test.Tasty

main :: IO ()
main = do
  eAppConf <- mkAppConfigFromEnv
  let tests_ = [DB.tests]
  tests <- case eAppConf of
    Right appConf -> pure $ tests_ ++ [Conversation.tests appConf]
    Left err -> do
      print err
      pure tests_
  defaultMain $ testGroup "Tests" tests
