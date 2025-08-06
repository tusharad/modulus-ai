{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.UserSubscription
  ( -- * User Subscription Marshallers
    userSubscriptionIDField
  , userSubscriptionOrganizationIDField
  , userSubscriptionPlanIDField
  , userSubscriptionStripeSubscriptionIDField
  , userSubscriptionCurrentPeriodEndsAtField
  , userSubscriptionCreatedAtField
  , userSubscriptionUpdatedAtField
  , userSubscriptionMarshaller
  ) where

import Data.Text (Text)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Utils (genRandomUuidDefault)
import Modulus.BE.DB.Internal.Marshaller.OrganizationMember
import Modulus.BE.DB.Internal.Marshaller.Organization
import Modulus.BE.DB.Internal.Marshaller.SubscriptionPlan (subscriptionStatusField)

-- User Subscription Fields
userSubscriptionIDField :: FieldDefinition NotNull UserSubscriptionID
userSubscriptionIDField =
  coerceField $
    setDefaultValue genRandomUuidDefault $
      uuidField "id"

userSubscriptionOrganizationIDField :: FieldDefinition NotNull OrganizationID
userSubscriptionOrganizationIDField = coerceField $ uuidField "organization_id"

userSubscriptionPlanIDField :: FieldDefinition NotNull SubscriptionPlanID
userSubscriptionPlanIDField = coerceField $ boundedTextField "plan_id" 255

userSubscriptionStripeSubscriptionIDField :: FieldDefinition Nullable (Maybe Text)
userSubscriptionStripeSubscriptionIDField =
  nullableField $ unboundedTextField "stripe_subscription_id"

userSubscriptionCurrentPeriodEndsAtField :: FieldDefinition Nullable (Maybe UTCTime)
userSubscriptionCurrentPeriodEndsAtField =
  nullableField $ utcTimestampField "current_period_ends_at"

userSubscriptionCreatedAtField :: FieldDefinition NotNull UTCTime
userSubscriptionCreatedAtField = organizationMemberCreatedAtField

userSubscriptionUpdatedAtField :: FieldDefinition NotNull UTCTime
userSubscriptionUpdatedAtField = organizationUpdatedAtField

-- User Subscription Marshaller
userSubscriptionMarshaller :: SqlMarshaller UserSubscriptionWrite UserSubscriptionRead
userSubscriptionMarshaller =
  UserSubscription
    <$> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionID)
          userSubscriptionIDField
      )
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionOrganizationID)
      userSubscriptionOrganizationIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionPlanID)
      userSubscriptionPlanIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionStripeSubscriptionID)
      userSubscriptionStripeSubscriptionIDField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionStatus)
      subscriptionStatusField
    <*> marshallField
      (\UserSubscription {..} -> userSubscriptionCurrentPeriodEndsAt)
      userSubscriptionCurrentPeriodEndsAtField
    <*> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionCreatedAt)
          userSubscriptionCreatedAtField
      )
    <*> marshallReadOnly
      ( marshallField
          (\UserSubscription {..} -> userSubscriptionUpdatedAt)
          userSubscriptionUpdatedAtField
      )
