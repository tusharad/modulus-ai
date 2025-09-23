module Modulus.BE.DB.Queries.RefreshTokens
  ( addRefreshToken
  , getRefreshToken
  , deleteRefreshToken
  , deleteRefreshTokensByUserID
  ) where

import qualified Data.Text as T
import Modulus.BE.DB.Internal.Marshaller.RefreshToken
  ( refreshTokenTokenHashField
  , refreshTokenUserIDField
  )
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table
import Orville.PostgreSQL

addRefreshToken :: (MonadOrville m) => RefreshTokenWrite -> m ()
addRefreshToken = insertEntity refreshTokenTable

getRefreshToken :: (MonadOrville m) => T.Text -> m (Maybe RefreshTokenRead)
getRefreshToken =
  findFirstEntityBy refreshTokenTable . where_ . fieldEquals refreshTokenTokenHashField

deleteRefreshToken :: (MonadOrville m) => RefreshTokenID -> m ()
deleteRefreshToken = deleteEntity refreshTokenTable

deleteRefreshTokensByUserID :: (MonadOrville m) => UserID -> m ()
deleteRefreshTokensByUserID uid =
  deleteEntities refreshTokenTable (Just $ fieldEquals refreshTokenUserIDField uid)
