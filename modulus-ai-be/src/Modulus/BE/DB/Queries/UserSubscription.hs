module Modulus.BE.DB.Queries.UserSubscription
  ( getUserSubscription
  , getUserSubscriptionByUserId
  , addUserSubscription
  , updateUserSubscription
  ) where

import Modulus.BE.DB.Internal.Marshaller (userSubscriptionUserIDField)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (userSubscriptionTable)
import Orville.PostgreSQL

getUserSubscription ::
  (MonadOrville m) =>
  UserSubscriptionID ->
  m (Maybe UserSubscriptionRead)
getUserSubscription = findEntity userSubscriptionTable

getUserSubscriptionByUserId ::
  (MonadOrville m) =>
  UserID ->
  m (Maybe UserSubscriptionRead)
getUserSubscriptionByUserId userId =
  findFirstEntityBy userSubscriptionTable $
    where_ (fieldEquals userSubscriptionUserIDField userId)

addUserSubscription ::
  (MonadOrville m) =>
  UserSubscriptionWrite ->
  m UserSubscriptionRead
addUserSubscription = insertAndReturnEntity userSubscriptionTable

updateUserSubscription ::
  (MonadOrville m) =>
  UserSubscriptionID ->
  UserSubscriptionWrite ->
  m ()
updateUserSubscription = updateEntity userSubscriptionTable
