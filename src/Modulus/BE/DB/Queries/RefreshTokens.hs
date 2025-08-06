module Modulus.BE.DB.Queries.RefreshTokens (
      addRefreshToken
    , getRefreshToken 
    , deleteRefreshToken 
    ) where

import Orville.PostgreSQL 
import Modulus.BE.DB.Internal.Model 
import Modulus.BE.DB.Internal.Table
import qualified Data.Text as T
import Modulus.BE.DB.Internal.Marshaller.RefreshToken (refreshTokenTokenHashField)

addRefreshToken :: MonadOrville m => RefreshTokenWrite -> m ()
addRefreshToken = insertEntity refreshTokenTable

getRefreshToken :: MonadOrville m => T.Text -> m (Maybe RefreshTokenRead)
getRefreshToken = 
    findFirstEntityBy refreshTokenTable . where_ . fieldEquals refreshTokenTokenHashField

deleteRefreshToken :: MonadOrville m => RefreshTokenID -> m ()
deleteRefreshToken = deleteEntity refreshTokenTable
