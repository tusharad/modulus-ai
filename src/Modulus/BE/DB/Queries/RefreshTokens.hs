module Modulus.BE.DB.Queries.RefreshTokens (
      addRefreshToken
    ) where

import Orville.PostgreSQL 
import Modulus.BE.DB.Internal.Model 
import Modulus.BE.DB.Internal.Table

addRefreshToken :: MonadOrville m => RefreshTokenWrite -> m ()
addRefreshToken = insertEntity refreshTokenTable
