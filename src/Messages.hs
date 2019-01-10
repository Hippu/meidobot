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
paskaMaailma t =
    (List.elem "paska" $ T.words t) && (List.elem "maailma" $ T.words t)