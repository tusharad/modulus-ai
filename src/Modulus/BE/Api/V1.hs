{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Modulus.BE.Api.V1
  ( API_V1
  , ConversationsAPI
  ) where

import Modulus.BE.Api.Internal.Auth (AuthAPI)
import Modulus.BE.Api.Types 
import Modulus.BE.Auth.JwtAuthCombinator (WithJWTAuth)
import Modulus.BE.DB.Internal.Model
import Servant

type API_V1 =
  "api" :> V1
type V1 = "v1" :> API

type API =
  "auth" :> AuthAPI
    :<|> "conversations" :> ConversationsAPI
    :<|> "health-check" :> Get '[JSON] String

type ConversationsAPI =
  WithJWTAuth
    :> ReqBody '[JSON] AddConversationRequest
        :> Get '[JSON] ConversationPublicID
    :<|> WithJWTAuth :> Post '[JSON] [ConversationRead]
    :<|> WithJWTAuth 
        :> Capture "conversationID" ConversationPublicID 
            :> ReqBody '[JSON] AddMessageRequest
                :> Post '[JSON] ()
    :<|> WithJWTAuth 
        :> Capture "conversationID" ConversationPublicID 
                :> Get '[JSON] [ChatMessageRead]
