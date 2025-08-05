module Modulus.BE.DB.Queries.EmailVerification
  ( addEmailVerificationOTP
  ) where

import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table
import Orville.PostgreSQL

addEmailVerificationOTP :: MonadOrville m => EmailVerificationOTPWrite -> m ()
addEmailVerificationOTP = insertEntity emailVerificationOTPTable
