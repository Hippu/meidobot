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
    | meidobotDiss t = Just "Vittu tapan sut"
    | otherwise = Nothing

messages :: User -> Message -> Maybe T.Text
messages botUser message =
    if (userId $ messageAuthor message) /= (userId botUser) then
        responseChooser $ T.toLower $ messageText message
    else
        Nothing

paskaMaailma :: T.Text -> Bool
paskaMaailma =
    hasAllWords ["paska", "maailma"]

meidobotDiss :: T.Text -> Bool
meidobotDiss text =
    hasAnyWords ["meidobot", "@meidobot", "robotti", "robotit", "botti"] text &&
    hasAnyWords ["paskaa", "tyhmä", "vittuun", "vitun", "paska", "idiootti", "idiootteja", "kiellettävä"] text

    
hasWord :: T.Text -> T.Text -> Bool
hasWord word text =
    List.elem word $ T.words text

hasAllWords :: [T.Text] -> T.Text -> Bool
hasAllWords words text =
    List.all (\x -> hasWord x text) words

hasAnyWords :: [T.Text] -> T.Text -> Bool
hasAnyWords words text =
    List.any (\x -> hasWord x text) words