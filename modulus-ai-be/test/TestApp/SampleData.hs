module TestApp.SampleData (insertData, InsertedData (..)) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import Data.Password.Bcrypt
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Queries.ChatMessage (addChatMessage)
import Modulus.BE.DB.Queries.Conversation (addConversation)
import Modulus.BE.DB.Queries.SubscriptionPlan (getAllSubscriptionPlans)
import Modulus.BE.DB.Queries.User (addUser)
import Modulus.BE.DB.Queries.UserSubscription (addUserSubscription)
import Orville.PostgreSQL (MonadOrville)

data InsertedData = InsertedData
  { insertedUser1ID :: UserID
  , insertedUser2ID :: UserID
  , insertedConvID :: BS.ByteString
  , insertedConvTitle :: T.Text
  , insertedUser3ID :: UserID
  }
  deriving (Show)

insertData :: MonadOrville m => m InsertedData
insertData = do
  -- User 1 (the owner of the conversation)
  pass1 <- hashPassword (mkPassword "user1pass")
  pass2 <- hashPassword (mkPassword "user2pass")
  pass3 <- hashPassword (mkPassword "user3pass")

  let user1Write =
        User
          { userID = ()
          , userEmail = "user1@test.com"
          , userHashedPassword = unPasswordHash pass1
          , userLastLoginAt = Nothing
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = True
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }
  -- User 2 (the intruder)
  let user2Write =
        User
          { userID = ()
          , userEmail = "user2@test.com"
          , userHashedPassword = unPasswordHash pass2
          , userLastLoginAt = Nothing
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = True
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }

  let user3Write =
        User
          { userID = ()
          , userEmail = "user3@test.com"
          , userHashedPassword = unPasswordHash pass3
          , userLastLoginAt = Nothing
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = True
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }

  user1 <- addUser user1Write
  user2 <- addUser user2Write
  user3 <- addUser user3Write

  mbSubscriptionPlanRead <- listToMaybe <$> getAllSubscriptionPlans
  case mbSubscriptionPlanRead of
    Nothing -> error "no values in subscription plans"
    Just subscriptionPlanRead -> do
      let newUserSubscription1 =
            UserSubscription
              { userSubscriptionID = ()
              , userSubscriptionUserID = userID user1
              , userSubscriptionPlanID = subscriptionPlanID subscriptionPlanRead
              , userSubscriptionStripeSubscriptionID = Nothing
              , userSubscriptionStatus = SubscriptionStatusActive
              , userSubscriptionCurrentPeriodEndsAt = Nothing
              , userSubscriptionCreatedAt = ()
              , userSubscriptionUpdatedAt = ()
              }
      let newUserSubscription2 =
            UserSubscription
              { userSubscriptionID = ()
              , userSubscriptionUserID = userID user2
              , userSubscriptionPlanID = subscriptionPlanID subscriptionPlanRead
              , userSubscriptionStripeSubscriptionID = Nothing
              , userSubscriptionStatus = SubscriptionStatusActive
              , userSubscriptionCurrentPeriodEndsAt = Nothing
              , userSubscriptionCreatedAt = ()
              , userSubscriptionUpdatedAt = ()
              }

      let newUserSubscription3 =
            UserSubscription
              { userSubscriptionID = ()
              , userSubscriptionUserID = userID user3
              , userSubscriptionPlanID = subscriptionPlanID subscriptionPlanRead
              , userSubscriptionStripeSubscriptionID = Nothing
              , userSubscriptionStatus = SubscriptionStatusActive
              , userSubscriptionCurrentPeriodEndsAt = Nothing
              , userSubscriptionCreatedAt = ()
              , userSubscriptionUpdatedAt = ()
              }

      _ <- addUserSubscription newUserSubscription1
      _ <- addUserSubscription newUserSubscription2
      _ <- addUserSubscription newUserSubscription3

      -- Conversation owned by User 1
      let convTitle = "Test Conversation Title"
      let conversationWrite = Conversation () () (Just $ userID user1) convTitle () ()
      conversationRead <- addConversation conversationWrite

      -- Message in User 1's conversation
      let chatMsgWrite =
            ChatMessage
              { chatMessageID = ()
              , chatMessagePublicID = ()
              , chatMessageConversationID = conversationID conversationRead
              , chatMessageRole = MessageRoleUser
              , chatMessageContent = "Hello World"
              , chatMessageModel = Nothing
              , chatMessageProvider = Nothing
              , chatMessagePromptTokens = Nothing
              , chatMessageCompletionTokens = Nothing
              , chatMessageCreatedAt = ()
              }
      void $ addChatMessage chatMsgWrite
      let (ConversationPublicID pubID_) = conversationPublicID conversationRead
      let pubID = UUID.toASCIIBytes pubID_

      pure $
        InsertedData
          { insertedUser1ID = userID user1
          , insertedUser2ID = userID user2
          , insertedConvID = pubID
          , insertedConvTitle = convTitle
          , insertedUser3ID = userID user3
          }
