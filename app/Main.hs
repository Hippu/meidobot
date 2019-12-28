
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception              ( finally )
import           Control.Monad                  ( when )
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import           Data.Char                      ( toLower )
import           Data.Monoid                    ( (<>) )
import           Messages
import           Discord
import           Discord.Types
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO


meidobot :: IO ()
meidobot = do
    tok <- T.strip <$> TIO.readFile "./secrets/auth-token.secret"
    outChan <- newChan :: IO (Chan String)
    threadId <- forkIO $ forever $ readChan outChan >>= putStrLn


    void $ runDiscord $ def { discordToken = tok
                            , discordOnEvent = meidoEventHandler outChan
                            , discordOnEnd = killThread threadId
                            , discordOnLog = print
                            }
    


meidoEventHandler :: Chan String -> DiscordHandle -> Event -> IO ()
meidoEventHandler chan dis event =
    case event of
        MessageCreate m -> messages dis m
        _               -> pure ()

main :: IO ()
main = meidobot
