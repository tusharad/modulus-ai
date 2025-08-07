{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.BE.Auth.JwtAuthCombinator
  ( WithJWTAuth
  , AuthResult (..)
  ) where

import Control.Lens (re, (^?), _Just)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Crypto.JWT
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Modulus.BE.DB.Internal.Model (UserID (UserID), UserRead)
import Modulus.BE.DB.Queries.User (getUser)
import Modulus.BE.Log (logDebug, logError)
import Modulus.BE.Monad.AppM (AppM, runAppM)
import Modulus.Common.Types
import Network.HTTP.Types (hAuthorization)
import Network.Wai (requestHeaders)
import Servant
import Servant.Client
import Servant.Client.Core.Request as ClientCore (Request, addHeader)
import Servant.Server.Internal
  ( DelayedIO
  , addAuthCheck
  )

data AuthResult
  = Authenticated UserRead
  | TokenExpired
  | InvalidToken
  | TokenNotFound
  | UserNotFound
  deriving (Show, Generic, ToJSON)

-- | Authenticate a request using Bearer Authentication
bearerAuthReq :: T.Text -> Request -> Request
bearerAuthReq token req =
  let authText = decodeUtf8 ("Bearer " <> TE.encodeUtf8 token)
   in ClientCore.addHeader "Authorization" authText req

instance HasClient m api => HasClient m (WithJWTAuth :> api) where
  type Client m (WithJWTAuth :> api) = T.Text -> Client m api

  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api) (bearerAuthReq val req)

  hoistClientMonad pm _ f cl bauth =
    hoistClientMonad pm (Proxy :: Proxy api) f (cl bauth)

data WithJWTAuth

instance
  (HasServer api ctx, HasContextEntry ctx AppConfig) =>
  HasServer (WithJWTAuth :> api) ctx
  where
  type ServerT (WithJWTAuth :> api) m = AuthResult -> ServerT api m

  route _ context subserver =
    route (Proxy :: Proxy api) context $
      subserver `addAuthCheck` jwtAuthCheck (getContextEntry context)

  hoistServerWithContext _ context nt s =
    hoistServerWithContext (Proxy :: Proxy api) context nt . s

-- | Extracts the token from a "Bearer <token>" header
parseBearerToken :: BSL.ByteString -> Maybe BSL.ByteString
parseBearerToken bs =
  case BSL.words bs of
    ["Bearer", token] -> Just token
    _ -> Nothing

-- | Extracts the subject (user UUID as Text) from a ClaimsSet
extractSubject :: ClaimsSet -> Maybe T.Text
extractSubject claims = claims ^? claimSub . _Just . re stringOrUri

audCheck :: StringOrURI -> Bool
audCheck aud =
  aud == expectedAud
  where
    expectedAud = "auth-service"

jwtAuthCheck :: AppConfig -> DelayedIO AuthResult
jwtAuthCheck appCfg = do
  mAuthHeader <- asks (lookup hAuthorization . requestHeaders)
  case mAuthHeader >>= (parseBearerToken . BSL.fromStrict) of
    Nothing -> do
      liftIO $ runAppM appCfg $ logError "Authorization header missing or invalid format"
      pure TokenNotFound
    Just rawToken -> do
      let key = fromOctets $ TE.encodeUtf8 (configJwtSecret appCfg)
      eClaims <- liftIO $ runJOSE $ verifyJWT_ key rawToken

      case eClaims of
        Left (_ :: JWTError) -> do
          liftIO $ runAppM appCfg $ logError "JWT verification failed"
          pure InvalidToken
        Right claims -> liftIO $ runAppM appCfg (handleClaims claims)
  where
    verifyJWT_ key token = do
      jwt <- decodeCompact token
      verifyClaims (defaultJWTValidationSettings audCheck) key jwt

    handleClaims :: ClaimsSet -> AppM AuthResult
    handleClaims claims =
      case extractSubject claims >>= UUID.fromText of
        Nothing -> do
          logError $ "Could not extract valid UUID from subject: " <> T.pack (show claims)
          pure InvalidToken
        Just uuid -> fetchUser (UserID uuid)

    fetchUser :: UserID -> AppM AuthResult
    fetchUser uid = do
      logDebug $ "Looking up user ID: " <> T.pack (show uid)
      result <- getUser uid
      pure $ maybe UserNotFound Authenticated result
