module Modulus.BE.DB.Queries.ApiKeys
  ( addApiKey
  , getApiKeysByUserId
  , getApiKeyByUserIdAndProviderName
  , updateApiKey
  ) where

import Data.Text (Text)
import Modulus.BE.DB.Internal.Marshaller
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table
import Orville.PostgreSQL

addApiKey :: (MonadOrville m) => ApiKeyWrite -> m ApiKeyRead
addApiKey = insertAndReturnEntity apiKeysTable

getApiKeysByUserId :: (MonadOrville m) => UserID -> m [ApiKeyRead]
getApiKeysByUserId userId =
  findEntitiesBy apiKeysTable $
    where_ (fieldEquals apiKeyUserIDField userId)

getApiKeyByUserIdAndProviderName :: (MonadOrville m) => UserID -> Text -> m (Maybe ApiKeyRead)
getApiKeyByUserIdAndProviderName userId providerName =
  findFirstEntityBy apiKeysTable $
    where_
      ( fieldEquals apiKeyUserIDField userId
          .&& fieldEquals apiKeyProviderNameField providerName
      )

updateApiKey :: (MonadOrville m) => ApiKeyID -> ApiKeyWrite -> m ()
updateApiKey = updateEntity apiKeysTable
