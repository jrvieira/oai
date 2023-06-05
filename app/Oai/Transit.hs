module Oai.Transit where

import GHC.Generics
import Data.Aeson

data Request = Request
   { model :: String
   , messages :: [Message]
   , temperature :: Maybe Double
   , top_p :: Maybe Double
   , n :: Maybe Int
   , stream :: Maybe Bool
   , stop :: Maybe [String]
   , max_tokens :: Maybe Int
   , presence_penalty :: Maybe Double
   , frequency_penalty :: Maybe Double
   , logit_bias :: Maybe (String,Int)
   , user :: Maybe String
   } deriving Generic

instance ToJSON Request where
   toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Message = Message
   { role :: Maybe String
   , content :: Maybe String
   , name :: Maybe String
   } deriving Generic

instance ToJSON Message where
   toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance FromJSON Message

data Response a = Response
   { id :: String
   , object :: String
   , created :: Int
   , model :: String
   , choices :: [a]
   , usage :: Maybe Usage
   } deriving Generic

instance FromJSON a => FromJSON (Response a)

data Choice = Choice
   { index :: Int
   , message :: Message
   , finish_reason :: String
   } deriving Generic

instance FromJSON Choice

data Delta = Delta
   { index :: Int
   , delta :: Message
   , finish_reason :: Maybe String
   } deriving Generic

instance FromJSON Delta

data Usage = Usage
   { prompt_tokens :: Int
   , completion_tokens :: Int
   , total_tokens :: Int
   } deriving Generic

instance FromJSON Usage

