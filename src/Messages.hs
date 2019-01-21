{-# LANGUAGE OverloadedStrings #-}
module Messages
    (messages
    ) where

import qualified Data.Text as T
import Discord
import qualified Data.List as List

messages :: (RestChan, Gateway, z) -> Message -> IO ()
messages dis receivedMessage =
    if not $ userIsBot $ messageAuthor receivedMessage then do
        case responseMessage receivedMessage of
            Just req -> do
                res <- restCall dis req
                print res
                putStrLn ""
            _ -> pure ()
        case responseReaction receivedMessage of
            Just reaction -> do
                res <- restCall dis $
                    CreateReaction (messageChannel receivedMessage, messageId receivedMessage)
                    reaction
                print res
                putStrLn ""
            _ -> pure ()
    else
        pure ()


responseMessage :: Message -> Maybe (ChannelRequest Message)
responseMessage m
    | T.isPrefixOf "moi" t = Just $ response "Moi!"
    | paskaMaailma t = Just $ response "http://gifs.hippuu.fi/g/112197.gif"
    | meidobotDiss t = Just $ response "Vittu tapan sut"
    | meidobotDiss2 m = Just $ response "http://gifs.hippuu.fi/g/mbot1.png"
    | otherwise = Nothing
    where
        t = T.toLower $ messageText m
        response = CreateMessage (messageChannel m)

responseReaction :: Message -> Maybe T.Text
responseReaction m
    | userName sender == "Jaagr" = Just "👎"
    | otherwise = Nothing
    where
        sender = messageAuthor m

paskaMaailma :: T.Text -> Bool
paskaMaailma =
    hasAllWords ["paska", "maailma"]

meidobotDiss :: T.Text -> Bool
meidobotDiss text =
    hasAnyWords ["meidobot", "robotti", "robotit", "botti"] text &&
    hasAnyWords ["paskaa", "tyhmä", "vittuun", "vitun", "paska", "idiootti", "idiootteja", "kiellettävä"] text

meidobotDiss2 :: Message -> Bool
meidobotDiss2 msg =
    msg `hasRecipientWithUserName` "Meidobot" &&
    T.toLower (messageText msg) `hasAnyWordsStartingWith`
    ["kuole", "haise", "ime", "vedä"]

hasRecipientWithUserName :: Message -> String -> Bool
hasRecipientWithUserName m recipientUserName =
    List.elem recipientUserName $ userName <$> messageMentions m

hasWord :: T.Text -> T.Text -> Bool
hasWord word text =
    List.elem word $ T.words text

hasWordStartingWith :: T.Text -> T.Text -> Bool
hasWordStartingWith text word =
    List.any (T.isPrefixOf word) $ T.words text

hasAllWords :: [T.Text] -> T.Text -> Bool
hasAllWords words text =
    List.all (`hasWord` text) words

hasAnyWords :: [T.Text] -> T.Text -> Bool
hasAnyWords words text =
    List.any (`hasWord` text) words

hasAnyWordsStartingWith :: T.Text -> [T.Text] -> Bool
hasAnyWordsStartingWith text =
    List.any (`hasWordStartingWith` text)