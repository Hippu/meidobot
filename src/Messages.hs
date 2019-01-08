{-# LANGUAGE OverloadedStrings #-}
module Messages
    (messages
    ) where

import qualified Data.Text as T
import Discord


responseChooser :: T.Text -> Maybe T.Text
responseChooser t
    | T.isPrefixOf "moi" t = Just "Moi!"
    | otherwise = Nothing

messages :: User -> Message -> Maybe T.Text
messages botUser message =
    if (userId $ messageAuthor message) /= (userId botUser) then
        responseChooser $ T.toLower $ messageText message
    else
        Nothing
