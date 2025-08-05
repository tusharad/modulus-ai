module Modulus.BE.DB.Queries.EmailVerification where

import Orville.PostgreSQL 
import Modulus.BE.DB.Internal.Model 
import Modulus.BE.DB.Internal.Table 

addEmailVerificationOTP :: MonadOrville m => EmailVerificationOTPWrite -> m ()
addEmailVerificationOTP = insertEntity emailVerificationOTPTable




