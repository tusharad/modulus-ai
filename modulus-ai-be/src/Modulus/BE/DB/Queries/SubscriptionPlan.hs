module Modulus.BE.DB.Queries.SubscriptionPlan
  ( getSubscriptionPlan
  , getAllSubscriptionPlans
  , addSubscriptionPlan
  ) where

import qualified Data.List.NonEmpty as NE
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table (subscriptionPlanTable)
import Orville.PostgreSQL
import qualified Orville.PostgreSQL.Execution as Expr
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Schema.TableDefinition as Expr

getSubscriptionPlan ::
  (MonadOrville m) =>
  SubscriptionPlanID ->
  m (Maybe SubscriptionPlanRead)
getSubscriptionPlan = findEntity subscriptionPlanTable

getAllSubscriptionPlans :: (MonadOrville m) => m [SubscriptionPlanRead]
getAllSubscriptionPlans = findEntitiesBy subscriptionPlanTable mempty

addSubscriptionPlan :: (MonadOrville m) => SubscriptionPlanWrite -> m ()
addSubscriptionPlan x =
  executeVoid InsertQuery $
    Expr.mkInsertExpr
      Expr.WithReturning
      subscriptionPlanTable
      (Just Expr.onConflictDoNothing)
      (x NE.:| [])
