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
import Modulus.Common.Types
import Servant
import Servant.Multipart

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
    :> Post '[JSON] ConversationPublicID
    :<|> WithJWTAuth :> Get '[JSON] [ConversationRead]
    :<|> WithJWTAuth
      :> Capture "conversationID" ConversationPublicID
      :> MultipartForm Mem AddMessageRequest
      :> Post '[JSON] ()
    :<|> WithJWTAuth
      :> Capture "conversationID" ConversationPublicID
      :> Get '[JSON] [ChatMessageRead]
    :<|> WithJWTAuth
      :> Capture "conversationID" ConversationPublicID
      :> "stream"
      :> ReqBody '[JSON] LLMRespStreamBody
      :> StreamPost NewlineFraming JSON (SourceIO LLMRespStream)
    :<|> WithJWTAuth
      :> Capture "conversationID" ConversationPublicID
      :> Delete '[JSON] ()
    :<|> "model_providers" :> Get '[JSON] [ModelProviders]
