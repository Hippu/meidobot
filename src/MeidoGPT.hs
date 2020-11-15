{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MeidoGPT (sendMessage) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.Text as T (Text)
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as R

newtype MeidoGPTMessage = MeidoGPTMessage {message :: T.Text} deriving (Show, Generic)

instance ToJSON MeidoGPTMessage

instance FromJSON MeidoGPTMessage

sendMessage :: T.Text -> IO T.Text
sendMessage msg =
  let request :: R.Req (R.JsonResponse MeidoGPTMessage)
      request = R.req R.POST (R.http "127.0.0.1") (R.ReqBodyJson (MeidoGPTMessage msg)) R.jsonResponse (R.port 8000)
   in R.runReq R.defaultHttpConfig request <&> message . R.responseBody
