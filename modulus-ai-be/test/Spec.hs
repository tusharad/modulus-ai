import qualified Conversation
import qualified DB

import Control.Monad (void)
import Data.Password.Bcrypt
import qualified Data.Text.IO as T
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Schema (autoMigrateQ)
import Modulus.BE.DB.Queries.ChatMessage (addChatMessage)
import Modulus.BE.DB.Queries.Conversation (addConversation)
import Modulus.BE.DB.Queries.User (addUser)
import Modulus.BE.Handler.Auth (generateJWT)
import Modulus.BE.Monad.AppM (runAppM)
import Modulus.BE.Monad.Utils
import Modulus.Common.Types
import Orville.PostgreSQL (MonadOrville, runOrvilleWithState)
import Test.Tasty

main :: IO ()
main = do
  eAppConf <- mkAppConfigFromEnv
  case eAppConf of
    Right appConf -> do
      putStrLn "running migration..."
      runOrvilleWithState (configOrvilleState appConf) autoMigrateQ
      uID <- runOrvilleWithState (configOrvilleState appConf) insertData
      eToken <- runAppM appConf (generateJWT uID)
      case eToken of
        Left _ -> putStrLn "error while generating token"
        Right tok -> do
          print tok
          let tests =
                [ DB.tests
                , Conversation.tests appConf
                ]
          defaultMain $ testGroup "Tests" tests
    Left err -> T.putStrLn err

insertData :: MonadOrville m => m UserID
insertData = do
  hashedPassword <- hashPassword (mkPassword "1234")
  let sampleUser =
        User
          { userID = ()
          , userEmail = "randomuser@gmail.com"
          , userHashedPassword = unPasswordHash hashedPassword
          , userLastLoginAt = Nothing
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = True
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }
  sampleUserRead <- addUser sampleUser
  let conversationWrite =
        Conversation
          { conversationID = ()
          , conversationPublicID = ()
          , conversationUserID = Just (userID sampleUserRead)
          , conversationTitle = "Some random title"
          , conversationCreatedAt = ()
          , conversationUpdatedAt = ()
          }
  conversationRead <- addConversation conversationWrite
  let chatMsgWrite =
        ChatMessage
          { chatMessageID = ()
          , chatMessagePublicID = ()
          , chatMessageConversationID = conversationID conversationRead
          , chatMessageRole = MessageRoleUser
          , chatMessageContent = "What is the meaning of life?"
          , chatMessageModel = Just "gemini-flash-1"
          , chatMessageProvider = Just "gemini"
          , chatMessagePromptTokens = Nothing
          , chatMessageCompletionTokens = Nothing
          , chatMessageCreatedAt = ()
          }
  void $ addChatMessage chatMsgWrite
  pure (userID sampleUserRead)
