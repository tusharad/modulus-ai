{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-
Email SDK for Modulus backend

This will be a thin wrapper around Mailgun API
-}
module Modulus.BE.Service.Email.Core
  ( sendVerificationEmail
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Req

newtype Email = Email {email :: Text}
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype From = From {email :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data MailGunReqBody = MailGunReqBody
  { from :: From
  , to :: [Email]
  , subject :: Text
  , html :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

sendVerificationEmail :: Text -> Text -> Text -> IO (Either BS.ByteString ())
sendVerificationEmail apiToken toEmail otp = runReq defaultHttpConfig $ do
    let url = https "api.mailgun.net" /: "v3" /: "mg.tushar-adhatrao.in" /: "messages"
    let authHeader = basicAuth "api" $ T.encodeUtf8 apiToken
    resp <- req POST url
        (ReqBodyUrlEnc $ mconcat
            [ "from" =: ("haskread@mg.tushar-adhatrao.in" :: Text)
            , "to" =: toEmail
            , "subject" =: ("Email verification for HaskRead platform" :: Text)
            , "html" =: verifyEmailHTMLContent otp
            ])
        bsResponse  -- Changed to get ByteString response
        authHeader
    let statusCode = responseStatusCode resp
    return $ if statusCode >= 200 && statusCode < 300
             then Right ()
             else Left $ responseBody resp


-- | HTML content for verification email
verifyEmailHTMLContent :: Text -> Text
verifyEmailHTMLContent otp =
  [i|
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Email Verification</title>
        <style>
            body { font-family: Arial, sans-serif; line-height: 1.6; }
            .container { max-width: 600px; margin: 0 auto; padding: 20px; }
            .header { padding: 20px; text-align: center; }
            .content { padding: 20px; }
            .otp { font-size: 32px; font-weight: bold; text-align: center; 
                   padding: 20px; margin: 20px 0; 
                   border-radius: 5px; letter-spacing: 4px; }
            .footer { text-align: center; padding: 20px; font-size: 12px; }
        </style>
    </head>
    <body>
        <div class="container">
            <div class="header">
                <h1>Email Verification</h1>
            </div>
            <div class="content">
                <p>Hello,</p>
                <p>Thank you for signing up! 
                Please use the following verification code to complete your registration:</p>
                <div class="otp">#{otp}</div>
                <p>This code will expire in 10 minutes for security reasons.</p>
                <p>If you didn't request this verification, please ignore this email.</p>
            </div>
            <div class="footer">
                <p>This is an automated message, please do not reply.</p>
            </div>
        </div>
    </body>
    </html>
|]
