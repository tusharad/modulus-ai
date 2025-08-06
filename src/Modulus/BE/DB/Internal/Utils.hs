module Modulus.BE.DB.Internal.Utils
  ( genRandomUuidDefault
  ) where

import Data.UUID (UUID)
import Orville.PostgreSQL
import qualified Orville.PostgreSQL.Expr as Expr

genRandomUuidDefault :: DefaultValue UUID
genRandomUuidDefault =
  rawSqlDefault $
    Expr.functionCall (Expr.functionName "gen_random_uuid") []
