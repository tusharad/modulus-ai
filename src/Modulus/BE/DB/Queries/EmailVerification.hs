module Modulus.BE.DB.Queries.EmailVerification
  ( addEmailVerificationOTP
  , getEmailVerificationOTPByUserId
  , deleteOtp 
  ) where

import Modulus.BE.DB.Internal.Marshaller (emailVerificationOTPUserIDField)
import Modulus.BE.DB.Internal.Model
import Modulus.BE.DB.Internal.Table
import Orville.PostgreSQL
import Data.UUID (UUID)

addEmailVerificationOTP :: MonadOrville m => EmailVerificationOTPWrite -> m ()
addEmailVerificationOTP = insertEntity emailVerificationOTPTable

getEmailVerificationOTPByUserId ::
  MonadOrville m => UserID -> m (Maybe EmailVerificationOTPRead)
getEmailVerificationOTPByUserId uID =
  findFirstEntityBy emailVerificationOTPTable $
    where_ (fieldEquals emailVerificationOTPUserIDField uID)

deleteOtp :: MonadOrville m => UUID -> m ()
deleteOtp = deleteEntity emailVerificationOTPTable
