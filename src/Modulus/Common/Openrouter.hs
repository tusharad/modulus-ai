{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Modulus.Common.Openrouter (
    getOpenRouterModelList
  , OpenRouterModel (..)
) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T

-- | Represents the top-level JSON response structure.
newtype OpenRouterModelsResponse = ModelsResponse
  { modelsData :: [OpenRouterModel]
  }
  deriving (Show, Eq, Generic)

instance FromJSON OpenRouterModelsResponse where
  parseJSON = withObject
    "OpenRouterModelsResponse"
    $ \v -> ModelsResponse <$> v .: "data"

-- | Represents a single model entry from the API.
data OpenRouterModel = Model
  { modelId :: Text
  , modelCanonicalSlug :: Text
  , modelHuggingFaceId :: Maybe Text -- Can be null/missing
  , modelName :: Text
  , modelCreated :: Int -- Unix timestamp
  , modelDescription :: Text
  , modelContextLength :: Int
  , modelArchitecture :: Architecture
  , modelPricing :: Pricing
  , modelTopProvider :: TopProvider
  , modelPerRequestLimits :: Maybe Aeson.Value -- Can be null, complex, or missing
  , modelSupportedParameters :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON OpenRouterModel where
  parseJSON = withObject "OpenRouterModel" $ \v ->
    Model
      <$> v .: "id"
      <*> v .: "canonical_slug"
      <*> v .:? "hugging_face_id" -- Use .:? for Maybe fields
      <*> v .: "name"
      <*> v .: "created"
      <*> v .: "description"
      <*> v .: "context_length"
      <*> v .: "architecture"
      <*> v .: "pricing"
      <*> v .: "top_provider"
      <*> v .:? "per_request_limits" -- Use .:? for Maybe fields
      <*> v .: "supported_parameters"

-- | Represents the architecture details of a model.
data Architecture = Architecture
  { archModality :: Text
  , archInputModalities :: [Text]
  , archOutputModalities :: [Text]
  , archTokenizer :: Text
  , archInstructType :: Maybe Text -- Can be null
  }
  deriving (Show, Eq, Generic)

instance FromJSON Architecture where
  parseJSON = withObject "Architecture" $ \v ->
    Architecture
      <$> v .: "modality"
      <*> v .: "input_modalities"
      <*> v .: "output_modalities"
      <*> v .: "tokenizer"
      <*> v .:? "instruct_type" -- Use .:? for Maybe fields

-- | Represents the pricing details of a model.
data Pricing = Pricing
  { pricePrompt :: Text
  , priceCompletion :: Text
  , priceRequest :: Maybe Text
  , priceImage :: Maybe Text
  , priceWebSearch :: Maybe Text
  , priceInternalReasoning :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Pricing where
  parseJSON = withObject "Pricing" $ \v ->
    Pricing
      <$> v .: "prompt"
      <*> v .: "completion"
      <*> v .:? "request"
      <*> v .:? "image"
      <*> v .:? "web_search"
      <*> v .:? "internal_reasoning"

-- | Represents the top provider details of a model.
data TopProvider = TopProvider
  { providerContextLength :: Maybe Int
  , providerMaxCompletionTokens :: Maybe Int -- Can be null
  , providerIsModerated :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON TopProvider where
  parseJSON = withObject "TopProvider" $ \v ->
    TopProvider
      <$> v .:? "context_length"
      <*> v .:? "max_completion_tokens" 
      <*> v .:? "is_moderated"

-- | Function to perform the GET request and parse the response.
getOpenRouterModelList :: IO (Either String [OpenRouterModel])
getOpenRouterModelList = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://openrouter.ai/api/v1/models"
  response <- httpLbs request manager
  let body = responseBody response :: ByteString
  case Aeson.eitherDecode body of
    Left err -> pure $ Left ("Error while parsing models: " <> err)
    Right res -> pure $ Right 
        (filter (T.isInfixOf "free" . modelId) $ modelsData res)
