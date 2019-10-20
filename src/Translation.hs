{-# LANGUAGE OverloadedStrings #-}
module Translation where
import           Network.HTTP.Req
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Default.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List                     as List


translateUrl = T.pack "api.cognitive.microsofttranslator.com"

data Translation = Translation { text :: T.Text, to :: T.Text} deriving (Eq, Show)
instance FromJSON Translation where
    parseJSON = withObject "Translation"
        $ \v -> Translation <$> v .: "text" <*> v .: "to"

data TranslationObject = TranslationResponse { translations :: [Translation]} deriving (Eq, Show)
instance FromJSON TranslationObject where
    parseJSON = withObject "TranslationObject"
        $ \v -> TranslationResponse <$> v .: "translations"

data TranslationRequest = TranslationRequest {text_ :: T.Text}
instance ToJSON TranslationRequest where
    toJSON (TranslationRequest t) = object ["Text" .= t]

translateRequest
    :: (MonadHttp m) => T.Text -> T.Text -> m (JsonResponse [TranslationObject])
translateRequest token text =
    let requestBody = ReqBodyJson [TranslationRequest { text_ = text }]
    in  req POST (https translateUrl /: "translate") requestBody jsonResponse
            $  header "Ocp-Apim-Subscription-Key" (encodeUtf8 token)
            <> "api-version"
            =: ("3.0" :: T.Text)
            <> "from"
            =: ("en" :: T.Text)
            <> "to"
            =: ("fi" :: T.Text)

translateToFi :: T.Text -> IO [TranslationObject]
translateToFi text = do
    token <- T.strip <$> TIO.readFile "./secrets/translation.secret"
    print $ "translating " <> text
    runReq def $ do
        res <- translateRequest token text
        return $ responseBody res

translationResponseToText :: [TranslationObject] -> T.Text
translationResponseToText response =
    T.toLower $ T.concat (fmap text $ translations $ List.head response)
