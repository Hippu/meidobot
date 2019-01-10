{-# LANGUAGE OverloadedStrings #-}
module Messages
    (messages
    ) where

import qualified Data.Text as T
import Discord
import qualified Data.List as List

responseChooser :: T.Text -> Maybe T.Text
responseChooser t
    | T.isPrefixOf "moi" t = Just "Moi!"
    | paskaMaailma t = Just "http://gifs.hippuu.fi/g/112197.gif"
    | otherwise = Nothing

messages :: User -> Message -> Maybe T.Text
messages botUser message =
    if (userId $ messageAuthor message) /= (userId botUser) then
        responseChooser $ T.toLower $ messageText message
    else
        Nothing

paskaMaailma :: T.Text -> Bool
paskaMaailma =
    hasWords ["paska", "maailma"]
    
hasWord :: T.Text -> T.Text -> Bool
hasWord word text =
    List.elem word $ T.words text

hasWords :: [T.Text] -> T.Text -> Bool
hasWords words text =
    List.all (\x -> hasWord x text) words