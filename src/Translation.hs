{-# LANGUAGE OverloadedStrings #-}

module Translation where

import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class
import Data.List as List
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Network.HTTP.Req

translateUrl = T.pack "api.cognitive.microsofttranslator.com"

data Translation = Translation {text :: T.Text, to :: T.Text} deriving (Eq, Show)

instance FromJSON Translation where
  parseJSON = withObject "Translation" $
    \v -> Translation <$> v .: "text" <*> v .: "to"

data TranslationObject = TranslationResponse {translations :: [Translation]} deriving (Eq, Show)

instance FromJSON TranslationObject where
  parseJSON = withObject "TranslationObject" $
    \v -> TranslationResponse <$> v .: "translations"

data TranslationRequest = TranslationRequest {text_ :: T.Text}

instance ToJSON TranslationRequest where
  toJSON (TranslationRequest t) = object ["Text" .= t]

data TranslationType
  = FiToEn
  | EnToFi

translateRequest ::
  (MonadHttp m) => TranslationType -> T.Text -> T.Text -> m (JsonResponse [TranslationObject])
translateRequest translationType token text =
  let requestBody = ReqBodyJson [TranslationRequest {text_ = text}]
   in req POST (https translateUrl /: "translate") requestBody jsonResponse $
        header "Ocp-Apim-Subscription-Key" (encodeUtf8 token)
          <> "api-version"
          =: ("3.0" :: T.Text)
          <> case translationType of
            FiToEn -> "from" =: ("fi" :: T.Text) <> "to" =: ("en" :: T.Text)
            EnToFi -> "from" =: ("en" :: T.Text) <> "to" =: ("fi" :: T.Text)

translateEnToFi :: T.Text -> IO [TranslationObject]
translateEnToFi text = do
  token <- T.strip <$> TIO.readFile "./secrets/translation.secret"
  print $ "translating " <> text <> " to finnish"
  runReq defaultHttpConfig $ do
    res <- translateRequest EnToFi token text
    return $ responseBody res

translateFiToEn :: T.Text -> IO [TranslationObject]
translateFiToEn text = do
  token <- T.strip <$> TIO.readFile "./secrets/translation.secret"
  print $ "translating " <> text <> " to english"
  runReq defaultHttpConfig $ do
    res <- translateRequest FiToEn token text
    return $ responseBody res

translationResponseToText :: [TranslationObject] -> T.Text
translationResponseToText response =
  T.toLower $ T.concat (fmap text $ translations $ List.head response)
