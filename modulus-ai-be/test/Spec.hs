import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.Tasty (defaultMain, testGroup)
import TestApp.Core (getTests)

main :: IO ()
main = do
  eTests <- getTests
  case eTests of
    Left err -> T.putStrLn $ "Error setting up tests: " <> T.pack err
    Right t -> defaultMain $ testGroup "All Tests" t
