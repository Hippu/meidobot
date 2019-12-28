{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Meidovision where

import           Control.Monad
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default.Class
import           GHC.Generics
import           Network.HTTP.Req               ( (=:) )
import qualified Network.HTTP.Req              as R
import qualified Data.Text                     as T
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString               as B
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8With
                                                )
import           Data.Text.Encoding.Error       ( lenientDecode )

data AnalyzeImageRequest =
    AnalyzeImageRequest
    { imageUrl :: T.Text
    , params :: AnalyzeImageParameters
    , headers :: AnalyzeImageHeaders }

data AnalyzeImageParameters =
    AnalyzeImageParameters
    { visualFeatures :: Maybe T.Text
    , details :: Maybe T.Text
    , language :: Maybe T.Text }

data AnalyzeImageHeaders =
    AnalyzeImageHeaders
    { contentType :: T.Text
    , subscriptionKey :: T.Text }

data AnalyzeImageResponse =
    AnalyzeImageResponse
    { categories :: Maybe [AnalyzeImageCategory]
    , adult :: Maybe AnalyzeImageAdult
    , tags :: Maybe [AnalyzeImageTag]
    , description :: Maybe AnalyzeImageDescription
    , requestId :: T.Text
    , metadata :: AnalyzeImageMetadata
    , faces :: Maybe [AnalyzeImageFace]
    , color :: Maybe AnalyzeImageColor
    , imageType :: Maybe AnalyzeImageType }
    deriving (Show)

instance FromJSON AnalyzeImageResponse where
    parseJSON = withObject "AnalyzeImage" $ \o -> do
        requestId   <- o .: "requestId"
        categories  <- o .:? "categories"
        adult       <- o .:? "adult"
        tags        <- o .:? "tags"
        description <- o .:? "description"
        metadata    <- o .: "metadata"
        faces       <- o .:? "faces"
        color       <- o .:? "color"
        imageType   <- o .:? "imageType"
        return AnalyzeImageResponse { .. }

data AnalyzeImageResponseError =
    AnalyzeImageResponseError
    { code :: T.Text
    , message :: T.Text
    , errorRequestId :: Maybe T.Text}

data AnalyzeImageCategory =
    AnalyzeImageCategory
    { categoryName :: T.Text
    , score :: Float
    , categoryDetails :: Maybe Details}
    deriving (Show)

instance FromJSON AnalyzeImageCategory where
    parseJSON = withObject "category" $ \o -> do
        categoryName    <- o .: "name"
        score           <- o .: "score"
        categoryDetails <- o .:? "detail"
        return AnalyzeImageCategory { .. }

data Details =
    Details
    { celebrities :: Maybe [CelebrityDetail]
    , landmarks :: Maybe [LandmarkDetail]
    } deriving (Show)

instance FromJSON Details where
    parseJSON = withObject "detail" $ \o -> do
        celebrities <- o .:? "celebrities"
        landmarks   <- o .:? "landmarks"
        return Details { .. }

data CelebrityDetail =
    CelebrityDetail
    { celebrityName :: T.Text
--    , celebrityFaceRectangle :: (Integer, Integer, Integer, Integer)
    , confidence :: Float }
    deriving (Show)

instance FromJSON CelebrityDetail where
    parseJSON = withObject "celebrity" $ \o -> do
        celebrityName <- o .: "name"
        confidence    <- o .: "confidence"
        return CelebrityDetail { .. }

data LandmarkDetail =
    LandmarkDetail
    { landmarkName :: T.Text
    , landmarkConfidence :: Float }
    deriving (Show)

instance FromJSON LandmarkDetail where
    parseJSON = withObject "landmark" $ \o -> do
        landmarkName       <- o .: "name"
        landmarkConfidence <- o .: "confidence"
        return LandmarkDetail { .. }


data AnalyzeImageAdult =
    AnalyzeImageAdult
    { isAdultContent :: Bool
    , isRacyContent :: Bool
    , adultScore :: Float
    , racyScore :: Float }
    deriving (Generic, Show)

instance FromJSON AnalyzeImageAdult

data AnalyzeImageTag =
    AnalyzeImageTag
    { tagName :: T.Text
    , tagConfidence :: Float }
    deriving (Show)

instance FromJSON AnalyzeImageTag where
    parseJSON = withObject "tag" $ \o -> do
        tagName       <- o .: "name"
        tagConfidence <- o .: "confidence"
        return AnalyzeImageTag { .. }

data AnalyzeImageDescription =
    AnalyzeImageDescription
    { descriptionTags :: [T.Text]
    , captions :: [DescriptionCaption] }
    deriving (Show)

instance FromJSON AnalyzeImageDescription where
    parseJSON = withObject "description" $ \o -> do
        descriptionTags <- o .: "tags"
        captions        <- o .: "captions"
        return AnalyzeImageDescription { .. }


data DescriptionCaption =
    DescriptionCaption
    { captionText :: T.Text
    , captionConfidence :: Float }
    deriving (Show)

instance FromJSON DescriptionCaption where
    parseJSON = withObject "caption" $ \o -> do
        captionText       <- o .: "text"
        captionConfidence <- o .: "confidence"
        return DescriptionCaption { .. }

data AnalyzeImageMetadata =
    AnalyzeImageMetadata
    { width :: Integer
    , height :: Integer
    , format :: T.Text }
    deriving (Generic, Show)

instance FromJSON AnalyzeImageMetadata

data AnalyzeImageFace =
    AnalyzeImageFace
    { age :: Integer
    , gender :: T.Text }
    deriving (Generic, Show)
--    , faceRectangle :: (Integer, Integer, Integer, Integer) }

instance FromJSON AnalyzeImageFace

data AnalyzeImageColor =
    AnalyzeImageColor
    { dominantColorForeground :: T.Text
    , dominantColorBackground :: T.Text
    , dominantColors :: [T.Text]
    , accentColor :: T.Text
    , isBWImg :: Bool }
    deriving (Generic, Show)

instance FromJSON AnalyzeImageColor

data AnalyzeImageType =
    AnalyzeImageType
    { clipArtType :: Integer
    , lineDrawingType :: Integer }
    deriving (Generic, Show)

instance FromJSON AnalyzeImageType

data AnalyzeImageRequestBody =
    AnalyzeImageRequestBody
    { url :: T.Text }
    deriving (Generic, Show)

instance ToJSON AnalyzeImageRequestBody

linkIsLegitImage :: T.Text -> IO Bool
linkIsLegitImage link = do
    case
            ( R.parseUrlHttp (encodeUtf8 link)
            , R.parseUrlHttps (encodeUtf8 link)
            )
        of
            (Just (url, options), _) -> do
                headers <- R.runReq R.defaultHttpConfig $ do
                    res <- R.req R.GET url R.NoReqBody R.ignoreResponse options
                    return $ R.responseHeader res "Content-Type"
                return $ case headers of
                    Just mime ->
                        "image/"
                            `T.isPrefixOf` decodeUtf8With lenientDecode mime
                    _ -> False
            (_, Just (url, options)) -> do
                headers <- R.runReq R.defaultHttpConfig $ do
                    res <- R.req R.GET url R.NoReqBody R.ignoreResponse options
                    return $ R.responseHeader res "Content-Type"
                return True
            _ -> return False


analyzeRequest :: T.Text -> T.Text -> R.Req (R.JsonResponse AnalyzeImageResponse)
analyzeRequest token url =
    R.req
            R.POST
            (    R.https "northeurope.api.cognitive.microsoft.com"
            R./: "vision"
            R./: "v1.0"
            R./: "analyze"
            )
            (R.ReqBodyJson $ AnalyzeImageRequestBody url)
            R.jsonResponse
        $  R.header "Ocp-Apim-Subscription-Key" (encodeUtf8 token)
        <> "visualFeatures"
        =: ("Categories,Tags,Description,Faces,ImageType,Color,Adult" :: T.Text)
        <> "details"
        =: ("Celebrities" :: T.Text)

executeAnalyzeRequestWith :: T.Text -> IO (Either R.HttpException AnalyzeImageResponse)
executeAnalyzeRequestWith body =
    T.strip <$> TIO.readFile "./secrets/meidovision.secret" >>= \token ->
        try $ R.runReq R.defaultHttpConfig $ analyzeRequest token body >>= \res ->
            return $ R.responseBody res

