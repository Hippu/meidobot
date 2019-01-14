
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Messages
import Discord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


meidobot :: IO ()
meidobot = do
    tok <- T.strip <$> TIO.readFile "./secrets/auth-token.secret"
    dis <- loginRestGateway (Auth tok)
    putStrLn "Running..."
    finally (loop dis)
            (stopDiscord dis)

loop :: (RestChan, Gateway, z) -> IO ()
loop dis = do
    e <- nextEvent dis
    Right cache <- readCache dis
    case e of
        Left er -> putStrLn ("Event error: " <> show er)
        Right (MessageCreate m) -> messages dis (_currentUser cache) m
    loop dis

main :: IO ()
main = meidobot