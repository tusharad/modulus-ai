module Modulus.BE.DB.Internal.Marshaller.OldConvSummary
  ( oldConvSummaryIDField
  , oldConvSummaryConversationIDField
  , oldConvSummarySummaryField
  , oldConvSummaryCreatedAtField
  , oldConvSummaryMarshaller
  ) where

-- \|
-- Copyright   : (c) 2024 Modulus AI, Inc.
-- License     : All rights reserved.
-- Maintainer  : tusharadhatrao@gmail.com
-- Stability   : experimental
-- Portability : unix
-- \|

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.User (userCreatedAtField)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

oldConvSummaryIDField :: FieldDefinition NotNull OldConvSummaryID
oldConvSummaryIDField = coerceField $ bigSerialField "old_conv_summary_id"

oldConvSummaryConversationIDField :: FieldDefinition NotNull ConversationID
oldConvSummaryConversationIDField = coerceField $ bigIntegerField "conversation_id"

oldConvSummarySummaryField :: FieldDefinition NotNull Text
oldConvSummarySummaryField = unboundedTextField "summary"

oldConvSummaryCreatedAtField :: FieldDefinition NotNull UTCTime
oldConvSummaryCreatedAtField = userCreatedAtField

oldConvSummaryMarshaller ::
  SqlMarshaller OldConvSummaryWrite OldConvSummaryRead
oldConvSummaryMarshaller =
  OldConvSummary
    <$> marshallReadOnly
      ( marshallField
          (\OldConvSummary {..} -> oldConvSummaryID)
          oldConvSummaryIDField
      )
    <*> marshallField
      (\OldConvSummary {..} -> oldConvSummaryConversationID)
      oldConvSummaryConversationIDField
    <*> marshallField
      (\OldConvSummary {..} -> oldConvSummarySummary)
      oldConvSummarySummaryField
    <*> marshallReadOnly
      ( marshallField
          (\OldConvSummary {..} -> oldConvSummaryCreatedAt)
          oldConvSummaryCreatedAtField
      )
