module Modulus.BE.DB.Internal.Marshaller.ApiKeys
  ( -- * API Key Marshallers
    apiKeyIDField
  , apiKeyUserIDField
  , apiKeyProviderNameField
  , apiKeyValField
  , apiKeyCreatedAtField
  , apiKeysMarshaller
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Modulus.BE.DB.Internal.Marshaller.User (userCreatedAtField)
import Modulus.BE.DB.Internal.Model
import Orville.PostgreSQL

apiKeyIDField :: FieldDefinition NotNull ApiKeyID
apiKeyIDField = coerceField $ serialField "api_key_id"

apiKeyUserIDField :: FieldDefinition NotNull UserID
apiKeyUserIDField = coerceField $ uuidField "user_id"

apiKeyProviderNameField :: FieldDefinition NotNull Text
apiKeyProviderNameField = unboundedTextField "provider_name"

apiKeyValField :: FieldDefinition NotNull Text
apiKeyValField = unboundedTextField "api_key_value"

apiKeyCreatedAtField :: FieldDefinition NotNull UTCTime
apiKeyCreatedAtField = userCreatedAtField

apiKeysMarshaller :: SqlMarshaller ApiKeyWrite ApiKeyRead
apiKeysMarshaller =
  ApiKeys
    <$> marshallReadOnly
      ( marshallField
          (\ApiKeys {..} -> apiKeyID)
          apiKeyIDField
      )
    <*> marshallField
      (\ApiKeys {..} -> apiKeyUserID)
      apiKeyUserIDField
    <*> marshallField
      (\ApiKeys {..} -> apiKeyProviderName)
      apiKeyProviderNameField
    <*> marshallField
      (\ApiKeys {..} -> apiKeyVal)
      apiKeyValField
    <*> marshallReadOnly
      ( marshallField
          (\ApiKeys {..} -> apiKeyCreatedAt)
          apiKeyCreatedAtField
      )
