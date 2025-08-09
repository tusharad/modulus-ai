{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modulus.BE.Handler.Auth
  ( authServer
  , registerHandler 
  , verifyOTPHandler
  , loginHandler
  , meHandler
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Crypto.JWT
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Password.Bcrypt
import qualified Data.String as Str
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4
import Modulus.BE.Api.Internal.Auth (AuthAPI)
import Modulus.BE.Api.Types
import Modulus.BE.Auth.JwtAuthCombinator (AuthResult (..))
import Modulus.BE.DB.Internal.Model
  ( EmailVerificationOTP (..)
  , RefreshToken (..)
  , User (..)
  , UserID (..)
  , UserRead
  )
import Modulus.BE.DB.Queries.EmailVerification
  ( addEmailVerificationOTP
  , deleteOtp
  , getEmailVerificationOTPByUserId
  )
import Modulus.BE.DB.Queries.RefreshTokens (addRefreshToken, deleteRefreshToken, getRefreshToken)
import Modulus.BE.DB.Queries.User (addUser, getUser, getUserByEmailQ, updateUser)
import Modulus.BE.Log (logDebug, logInfo)
import Modulus.BE.Monad.AppM
import Modulus.BE.Monad.Error
import Modulus.BE.Monad.Utils
import Modulus.BE.Service.Email.Core (sendVerificationEmail)
import qualified Orville.PostgreSQL as Orville
import Servant
import System.Random
import Text.Email.Validate
import Modulus.Common.Types (AppConfig(configMailGunApiKey, configJwtSecret))

authServer :: ServerT AuthAPI AppM
authServer =
  registerHandler
    :<|> loginHandler
    :<|> verifyOTPHandler
    :<|> refreshTokenHandler
    :<|> meHandler

refreshTokenHandler :: RefreshTokenRequest -> AppM AuthTokens
refreshTokenHandler RefreshTokenRequest {..} = do
  mbRefreshToken <- getRefreshToken refreshToken
  case mbRefreshToken of
    Nothing -> throwError $ ValidationError "Token not found"
    Just RefreshToken {..} -> do
      logDebug $ "refresh token for user: " <> T.pack (show refreshTokenUserID)
      deleteRefreshToken refreshTokenID
      t <- liftIO getCurrentTime
      when
        (refreshTokenExpiresAt < t || refreshTokenIsRevoked)
        (throwError $ ValidationError "Token Expired or revoked")
      mbUser <- getUser refreshTokenUserID
      case mbUser of
        Nothing -> throwError $ ValidationError "User not found for token"
        Just User {..} -> do
          jwtToken <- generateJWT userID
          refreshToken_ <- generateAndStoreRefreshToken userID
          pure $
            AuthTokens
              { accessToken = TE.decodeUtf8 $ BSL.toStrict jwtToken
              , refreshToken = refreshToken_
              }

verifyUser :: UserRead -> AppM ()
verifyUser user = do
  let updatedUser =
        user
          { userID = ()
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = True
          }
  updateUser (userID user) updatedUser

verifyOTPSanityCheck :: OTPVerifyRequest -> AppM ()
verifyOTPSanityCheck OTPVerifyRequest {..} = do
  mbUser <- getUserByEmailQ verifyEmail
  case mbUser of
    Nothing -> throwError $ ValidationError "User not found"
    Just user@User {..} -> do
      when userIsEmailVerified (throwError $ ValidationError "User already verified")
      mbEmailVerification <- getEmailVerificationOTPByUserId userID
      case mbEmailVerification of
        Nothing -> throwError $ ValidationError "OTP not found"
        Just EmailVerificationOTP {..} -> do
          t <- liftIO getCurrentTime
          when
            (t > emailVerificationOTPExpiresAt)
            (throwError $ ValidationError "Verification code has expired.")
          let passwordMatches =
                checkPassword
                  (mkPassword . T.pack $ show verifyOTP)
                  (PasswordHash emailVerificationOTPOtpHash)
          if passwordMatches == PasswordCheckSuccess
            then do
              logDebug $ "OTP verified: " <> userEmail
              Orville.withTransaction $ do
                verifyUser user
                deleteOtp emailVerificationOTPID
            else throwError $ ValidationError "Invalid verification code."

verifyOTPHandler :: OTPVerifyRequest -> AppM T.Text
verifyOTPHandler otpVerifyReq = do
  verifyOTPSanityCheck otpVerifyReq
  pure "Email verified successfully. You may now log in."

meHandler :: AuthResult -> AppM T.Text
meHandler (Authenticated user) = return $ "Hello, " <> userEmail user
meHandler TokenExpired = throwError $ AuthenticationError "Token expired"
meHandler InvalidToken = throwError $ AuthenticationError "Invalid token"
meHandler r = throwError $ AuthenticationError $ "User not found " <> T.pack (show r)

isValidPassword :: String -> Bool
isValidPassword pwd =
  length pwd >= 8
    && any isAsciiUpper pwd
    && any isAsciiLower pwd
    && any isDigit pwd

registerSanityCheck :: RegisterRequest -> AppM ()
registerSanityCheck RegisterRequest {..} = do
  -- Check password confirmation
  when (registerPassword /= registerConfirmPassword) $
    throwError $
      AuthenticationError "Password and confirm password do not match"

  -- Validate email format
  when (isLeft $ validate $ TE.encodeUtf8 registerEmail) $
    throwError $
      AuthenticationError "Invalid email format"

  -- Validate password strength
  unless (isValidPassword $ T.unpack registerPassword) $
    throwError $
      AuthenticationError "Password does not meet requirements"

  -- Check for existing user
  existingUser <- getUserByEmailQ registerEmail
  when (isJust existingUser) $
    throwError $
      AuthenticationError "Account with this email already exists"

genAndStoreEmailOTP :: UserID -> AppM Int
genAndStoreEmailOTP userID = do
  otp <- liftIO $ randomRIO (100000, 999999)
  hashedOtp <- hashPassword (mkPassword (T.pack $ show otp))
  t <- liftIO getCurrentTime
  let expiry = addUTCTime 600 t -- 10 minutes
  let emailOTPWrite =
        EmailVerificationOTP
          { emailVerificationOTPID = ()
          , emailVerificationOTPUserID = userID
          , emailVerificationOTPOtpHash = unPasswordHash hashedOtp
          , emailVerificationOTPExpiresAt = expiry
          , emailVerificationOTPCreatedAt = ()
          }
  addEmailVerificationOTP emailOTPWrite
  return otp

sendEmailAsync :: T.Text -> T.Text -> T.Text -> IO ()
sendEmailAsync apiKey email otp = do
  let trySend 0 = print $ "Failed to send verification email to " <> email
      trySend attemptsLeft = do
        result <- sendVerificationEmail apiKey email otp
        case result of
          Right _ -> print $ "Successfully sent verification email to " <> email
          Left err -> do
            print ("mailgun api error:" :: String, err)
            threadDelay 20000
            trySend (attemptsLeft - 1)
  trySend (2 :: Int)

registerHandler :: RegisterRequest -> AppM UserProfile
registerHandler registerReq@RegisterRequest {..} = do
  logDebug "New user registration attempt"
  registerSanityCheck registerReq
  hashedPassword <- hashPassword (mkPassword registerPassword)
  let newUser =
        User
          { userID = ()
          , userEmail = registerEmail
          , userHashedPassword = unPasswordHash hashedPassword
          , userLastLoginAt = Nothing
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userIsEmailVerified = False
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }
  newUserRead <- addUser newUser
  otp <- genAndStoreEmailOTP (userID newUserRead)
  emailApiKey <- asks configMailGunApiKey
  liftIO $
    void $
      forkIO $
        sendEmailAsync emailApiKey (userEmail newUserRead) (T.pack $ show otp)
  logDebug "query successful"
  return $
    UserProfile
      { userProfileId = userID newUserRead
      , userProfileEmail = userEmail newUserRead
      }

isAccountLocked :: UserRead -> UTCTime -> Bool
isAccountLocked User {..} now =
  case userLockedUntil of
    Just lockTime -> now < lockTime
    Nothing -> False

resetFailedLoginAttempts :: UserRead -> AppM ()
resetFailedLoginAttempts user = do
  let updatedUser =
        user
          { userID = ()
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userFailedLoginAttempts = 0
          , userLockedUntil = Nothing
          }
  updateUser (userID user) updatedUser

incrementFailedAttempt :: UserRead -> AppM ()
incrementFailedAttempt user = do
  let updatedUser =
        user
          { userID = ()
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userFailedLoginAttempts = userFailedLoginAttempts user + 1
          , userLockedUntil = Nothing
          }
  updateUser (userID user) updatedUser

lockUser :: UserRead -> AppM ()
lockUser user = do
  t <- liftIO getCurrentTime
  let expiry = addUTCTime 600 t -- 10 minutes
  let updatedUser =
        user
          { userID = ()
          , userCreatedAt = ()
          , userUpdatedAt = ()
          , userLockedUntil = Just expiry
          }
  updateUser (userID user) updatedUser

mkClaims :: UUID -> IO ClaimsSet
mkClaims userID = do
  t <- getCurrentTime
  let expiry = addUTCTime 3600 t -- 1 hour
  pure $
    emptyClaimsSet
      & claimIss ?~ "modulus-ai"
      & claimAud ?~ Audience ["auth-service"]
      & claimIat ?~ NumericDate t
      & claimExp ?~ NumericDate expiry
      & claimSub ?~ Str.fromString (UUID.toString userID)

generateJWT :: UserID -> AppM BSL.ByteString
generateJWT (UserID uID) = do
  claimSet <- liftIO $ mkClaims uID
  secret <- asks configJwtSecret
  eSignedJWT <- liftIO $ runJOSE $ do
    signClaims (hmacJwk secret) (newJWSHeader ((), HS256)) claimSet
  case eSignedJWT of
    Left (x :: JWTError) -> do
      logDebug $ T.pack (show x)
      throwError $ ValidationError "Could not create JWT token"
    Right signJwt -> do
      pure $ encodeCompact signJwt

generateAndStoreRefreshToken :: UserID -> AppM T.Text
generateAndStoreRefreshToken uID = do
  someUUID <- liftIO nextRandom
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (7 * 24 * 3600) now -- 7 days
  let rt =
        RefreshToken
          { refreshTokenUserID = uID
          , refreshTokenTokenHash = T.pack $ UUID.toString someUUID
          , refreshTokenExpiresAt = expiry
          , refreshTokenIsRevoked = False
          , refreshTokenID = ()
          , refreshTokenCreatedAt = ()
          }
  addRefreshToken rt
  pure (T.pack $ UUID.toString someUUID)

loginSanityCheck :: LoginRequest -> AppM UserRead
loginSanityCheck LoginRequest {..} = do
  -- valid email
  when (isLeft $ validate (TE.encodeUtf8 loginEmail)) $
    throwError $
      AuthenticationError "Invalid email format"

  -- check if user exists
  mUser <- getUserByEmailQ loginEmail
  user@User {..} <- case mUser of
    Nothing -> do
      logDebug "Login failed: user not found"
      throwError $ AuthenticationError "Invalid credentials"
    Just u -> pure u

  -- check if account is locked or not
  now <- liftIO getCurrentTime
  when (isAccountLocked user now) $ do
    logDebug $ "Login failed: account locked for user: " <> T.pack (show userID)
    throwError $ AuthenticationError "Account is locked. Try again later."

  -- check if account is verified
  unless userIsEmailVerified $ do
    logDebug $ "Login failed: unverified email for user: " <> T.pack (show userID)
    throwError $ AuthenticationError "Email not verified"

  let passwordMatches =
        checkPassword (mkPassword loginPassword) (PasswordHash userHashedPassword)
  if passwordMatches == PasswordCheckSuccess
    then do
      logDebug $ "Sanity check successful: " <> T.pack (show userID)
      pure user
    else do
      incrementFailedAttempt user
      when (userFailedLoginAttempts + 1 > 5) (lockUser user)
      throwError $ AuthenticationError "Invalid cred"

loginHandler :: LoginRequest -> AppM AuthTokens
loginHandler loginReq = do
  logInfo "New user login attempt"
  user@User {..} <- loginSanityCheck loginReq
  -- Reset failed attempts on success
  when (userFailedLoginAttempts > 0) (resetFailedLoginAttempts user)
  -- Generate tokens
  jwtToken <- generateJWT userID
  refreshToken <- generateAndStoreRefreshToken userID
  logDebug $ "Login successful: " <> T.pack (show userID)
  pure $
    AuthTokens
      { accessToken = TE.decodeUtf8 $ BSL.toStrict jwtToken
      , refreshToken = refreshToken
      }
