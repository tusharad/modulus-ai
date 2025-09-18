module Modulus.BE.DB.Queries.SubscriptionPlan
  ( getSubscriptionPlan
  , getAllSubscriptionPlans
  , addSubscriptionPlan
  ) where

import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (subscriptionPlanTable)
import Orville.PostgreSQL

getSubscriptionPlan :: (MonadOrville m) => SubscriptionPlanID -> m (Maybe SubscriptionPlan)
getSubscriptionPlan = findEntity subscriptionPlanTable

getAllSubscriptionPlans :: (MonadOrville m) => m [SubscriptionPlan]
getAllSubscriptionPlans = findEntitiesBy subscriptionPlanTable mempty

addSubscriptionPlan :: (MonadOrville m) => SubscriptionPlan -> m SubscriptionPlan
addSubscriptionPlan = insertAndReturnEntity subscriptionPlanTable
