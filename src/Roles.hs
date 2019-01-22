{-# LANGUAGE OverloadedStrings #-}

module Roles (roles) where

import Discord
import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.List as List

getRoles :: IO (Maybe (Map.Map T.Text Snowflake))
getRoles = do
    roleJSON <- B.readFile "./data/roles.json"
    return $ fmap (Map.map toSnowflake) $ decode roleJSON

joinRole :: (RestChan, Gateway, z) -> (GuildId, User) -> RoleId -> IO ()
joinRole dis user roleID = do
    -- let newRoles = onlyOneRoleOfAvailable (memberRoles member) roleID (List.map snd $ Map.toList roles)
    putStrLn ""

onlyOneRoleOfAvailable :: [Snowflake] -> Snowflake -> [Snowflake] -> [Snowflake]
onlyOneRoleOfAvailable currentRoles newRole availableRoles =
    (currentRoles List.\\ availableRoles) ++ [newRole]
    
toSnowflake :: String -> Snowflake
toSnowflake snowflake =
    Snowflake (read snowflake)

roles :: (RestChan, Gateway, z) -> Message -> IO ()
roles dis message = do
    let guild = messageGuild message
    let user = messageAuthor message
    let text = messageText message
    Just availableRoles <- getRoles
    let rid = Map.lookup text availableRoles
    case (guild, rid) of
        (Just g, Just r) -> do
            joinRole dis (g, user) r
        _ -> putStrLn ""
    
