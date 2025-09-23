module TestApp.SampleData (insertData) where

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

insertData :: MonadOrville m => m (UserID, UserID, BS.ByteString, T.Text)
insertData = do
  -- User 1 (the owner of the conversation)
  pass1 <- hashPassword (mkPassword "user1pass")
  let user1Write =
        User
          ()
          "user1@test.com"
          (unPasswordHash pass1)
          Nothing
          ()
          ()
          True
          0
          Nothing
  user1 <- addUser user1Write

  -- User 2 (the intruder)
  pass2 <- hashPassword (mkPassword "user2pass")
  let user2Write =
        User
          ()
          "user2@test.com"
          (unPasswordHash pass2)
          Nothing
          ()
          ()
          True
          0
          Nothing
  user2 <- addUser user2Write

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
      _ <- addUserSubscription newUserSubscription1

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
      _ <- addUserSubscription newUserSubscription2

      -- Conversation owned by User 1
      let convTitle = "Test Conversation Title"
      let conversationWrite = Conversation () () (Just $ userID user1) convTitle () ()
      conversationRead <- addConversation conversationWrite

      -- Message in User 1's conversation
      let chatMsgWrite =
            ChatMessage
              ()
              ()
              (conversationID conversationRead)
              MessageRoleUser
              "Hello World"
              Nothing
              Nothing
              Nothing
              Nothing
              ()
      void $ addChatMessage chatMsgWrite
      let (ConversationPublicID pubID_) = conversationPublicID conversationRead
      let pubID = UUID.toASCIIBytes pubID_
      pure
        ( userID user1
        , userID user2
        , pubID
        , conversationTitle conversationRead
        )
