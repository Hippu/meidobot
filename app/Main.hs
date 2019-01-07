
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Lib
import Discord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


helloWorld :: IO ()
helloWorld = do
    tok <- T.strip <$> TIO.readFile "./secrets/auth-token.secret"
    dis <- loginRestGateway (Auth tok)
    finally (loopingHello dis)
            (stopDiscord dis)

loopingHello :: (RestChan, Gateway, z) -> IO ()
loopingHello dis = do
    e <- nextEvent dis
    case e of
        Left er -> putStrLn ("Event error: " <> show er)
        Right (MessageCreate m) -> do
            when (isHello (messageText m)) $ do
                resp <- restCall dis (CreateMessage (messageChannel m) "on kakkapylly.")
                putStrLn (show resp)
                putStrLn ""
        _ -> pure ()
    loopingHello dis

isHello :: T.Text -> Bool
isHello = T.isPrefixOf "janne" . T.map toLower

main = helloWorld