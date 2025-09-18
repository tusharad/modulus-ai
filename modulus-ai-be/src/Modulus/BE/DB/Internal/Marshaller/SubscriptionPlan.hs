{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Modulus.BE.DB.Internal.Marshaller.SubscriptionPlan
  ( -- * Subscription Plan Marshallers
    subscriptionPlanIDField
  , subscriptionPlanNameField
  , subscriptionPlanPriceCentsField
  , subscriptionPlanFeaturesField
  , subscriptionPlanMarshaller

    -- * Subscription Status Marshaller
  , subscriptionStatusField
  , subscriptionPlanIDFieldFunc
  ) where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

-- Subscription Plan Fields
subscriptionPlanIDField :: FieldDefinition NotNull SubscriptionPlanID
subscriptionPlanIDField = subscriptionPlanIDFieldFunc "subscription_plan_id"

subscriptionPlanIDFieldFunc :: String -> FieldDefinition NotNull SubscriptionPlanID
subscriptionPlanIDFieldFunc = fieldOfType subscriptionPlanIDSqlType

subscriptionPlanIDSqlType :: SqlType SubscriptionPlanID
subscriptionPlanIDSqlType =
  tryConvertSqlType convertPlanIDToString convertStringToPlanID integer
  where
    convertPlanIDToString :: SubscriptionPlanID -> Int32
    convertPlanIDToString Free = 0
    convertPlanIDToString Gold = 1
    convertPlanIDToString Premium = 2

    convertStringToPlanID :: Int32 -> Either String SubscriptionPlanID
    convertStringToPlanID 0 = Right Free
    convertStringToPlanID 1 = Right Gold
    convertStringToPlanID 2 = Right Premium
    convertStringToPlanID s = Left $ "Invalid SubscriptionPlanID value: " ++ show s

subscriptionPlanNameField :: FieldDefinition NotNull Text
subscriptionPlanNameField = unboundedTextField "name"

subscriptionPlanPriceCentsField :: FieldDefinition NotNull Int32
subscriptionPlanPriceCentsField = integerField "price_cents"

subscriptionPlanFeaturesField :: FieldDefinition Nullable (Maybe Text)
subscriptionPlanFeaturesField = nullableField $ jsonbField "features"

subscriptionStatusSqlType :: SqlType SubscriptionStatus
subscriptionStatusSqlType =
  tryConvertSqlType convertStatusToString convertStringToStatus unboundedText
  where
    convertStatusToString :: SubscriptionStatus -> Text
    convertStatusToString SubscriptionStatusActive = "active"
    convertStatusToString SubscriptionStatusPastDue = "past_due"
    convertStatusToString SubscriptionStatusCanceled = "canceled"
    convertStatusToString SubscriptionStatusTrialing = "trialing"

    convertStringToStatus :: Text -> Either String SubscriptionStatus
    convertStringToStatus "active" = Right SubscriptionStatusActive
    convertStringToStatus "past_due" = Right SubscriptionStatusPastDue
    convertStringToStatus "canceled" = Right SubscriptionStatusCanceled
    convertStringToStatus "trialing" = Right SubscriptionStatusTrialing
    convertStringToStatus s = Left $ "Invalid SubscriptionStatus value: " ++ T.unpack s

subscriptionStatusFieldFunc :: String -> FieldDefinition NotNull SubscriptionStatus
subscriptionStatusFieldFunc = fieldOfType subscriptionStatusSqlType

-- Subscription Status Field
subscriptionStatusField :: FieldDefinition NotNull SubscriptionStatus
subscriptionStatusField = subscriptionStatusFieldFunc "status"

-- Subscription Plan Marshaller
subscriptionPlanMarshaller :: SqlMarshaller SubscriptionPlan SubscriptionPlan
subscriptionPlanMarshaller =
  SubscriptionPlan
    <$> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanID)
      subscriptionPlanIDField
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanName)
      subscriptionPlanNameField
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanPriceCents)
      subscriptionPlanPriceCentsField
    <*> marshallField
      (\SubscriptionPlan {..} -> subscriptionPlanFeatures)
      subscriptionPlanFeaturesField
