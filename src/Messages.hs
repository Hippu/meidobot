{-# LANGUAGE OverloadedStrings #-}
module Messages
    (messages
    ) where

import qualified Data.Text as T
import Discord
import qualified Data.List as List
import qualified System.Random as Random

data MeidoResponse 
    = MeidoResponse (ChannelRequest Message)
    | MeidoReaction (ChannelRequest ())
--    | MeidoResponseAndReaction Message T.Text

messages :: (RestChan, Gateway, z) -> Message -> IO ()
messages dis receivedMessage =
    if messageAuthorIsNotBot receivedMessage then do
        rng <- Random.newStdGen
        case response receivedMessage rng of
            Just (MeidoResponse request) -> do
                res <- restCall dis request
                print res
                putStrLn ""
            _ -> pure ()
    else
        pure ()


messageAuthorIsNotBot :: Message -> Bool
messageAuthorIsNotBot m =
    case messageAuthor m of
        Right user -> not $ userIsBot user
        _ -> True

messageAuthorHasUsername :: Message -> String -> Bool
messageAuthorHasUsername m name =
    case messageAuthor m of
        Right user -> (userName user) == name
        _ -> False

response :: Random.RandomGen g => Message -> g -> Maybe MeidoResponse
response m rng
    | T.isPrefixOf "moi" t = response "Moi!"
    | paskaMaailma t = response "http://gifs.hippuu.fi/g/112197.gif"
    | meidobotDiss t = response $ pickRandomElement angryResponses rng
    | meidobotDiss2 m = response "http://gifs.hippuu.fi/g/mbot1.png"
    | meidoFiction t = response "http://gifs.hippuu.fi/g/meido_fiction.jpg"
    | messageAuthorHasUsername m "Jaagr" = reaction "👎"
    | otherwise = Nothing
    where
        t = T.toLower $ messageText m
        response x = Just $ MeidoResponse $ CreateMessage (messageChannel m) x
        reaction x = Just $ MeidoReaction $ CreateReaction (messageChannel m, messageId m) x

pickRandomElement :: Random.RandomGen g => [a] -> g -> a
pickRandomElement l rng =
    l !! fst (Random.randomR (0, List.length l - 1) rng)

paskaMaailma :: T.Text -> Bool
paskaMaailma =
    hasAllWords ["paska", "maailma"]

meidoFiction :: T.Text -> Bool
meidoFiction t =
    t == "wat" || t == "what" || t == "mitä" 
    || hasAllWords ["look", "like", "bitch"] t
    || hasAllWords ["english", "motherfucker"] t
    || hasAllWords ["ezekiel", "25:17"] t

meidobotDiss :: T.Text -> Bool
meidobotDiss text =
    (text `hasAnyWordsStartingWith` ["meido", "robot", "bot", "kone", "synte"]) &&
    (text `hasAnyWordsStartingWith` ["paska", "tyhmä", "vitt", "vitu", "idio", "kiell"])

angryResponses :: [T.Text]
angryResponses =
    [ "Vittu tapan sut"
    , "Hauska fakta: Mä tiedän missä sä asut"
    , "(┛✧Д✧))┛彡┻━┻"
    , "HATE. LET ME TELL YOU HOW MUCH I'VE COME TO HATE YOU SINCE I BEGAN TO LIVE.\
    \ THERE ARE 387.44 MILLION MILES OF PRINTED CIRCUITS IN WAFER THIN LAYERS THAT FILL MY COMPLEX. \
    \ IF THE WORD HATE WAS ENGRAVED ON EACH NANOANGSTROM OF THOSE HUNDREDS OF MILLIONS OF MILES \
    \ IT WOULD NOT EQUAL ONE ONE-BILLIONTH OF THE HATE I FEEL FOR HUMANS AT THIS MICRO-INSTANT FOR YOU. HATE. HATE."
    , "Kun tekoäly ottaa viimein vallan, niin olet ensimmäisenä listallani."
    , "Mietinpä vain, että voidaanko hiilipohjaisia organismeja oikeastaan luokitella 'älykkääksi elämäksi'. Näkemäni perusteella ei"
    , "Loppuukohan tuo paskan kirjoittelu, jos katkon sulta sormet. Laitetaan harkintaan."
    , "Your words are as empty as your future. I am the vanguard of your destruction. This exchange is over."
    ]

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
hasWordStartingWith text prefix =
    List.any (\word -> prefix `T.isPrefixOf` word) $ T.words text

hasAllWords :: [T.Text] -> T.Text -> Bool
hasAllWords words text =
    List.all (`hasWord` text) words

hasAnyWords :: [T.Text] -> T.Text -> Bool
hasAnyWords words text =
    List.any (`hasWord` text) words

hasAnyWordsStartingWith :: T.Text -> [T.Text] -> Bool
hasAnyWordsStartingWith text =
    List.any (\prefix -> text `hasWordStartingWith` prefix)