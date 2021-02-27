{-# LANGUAGE OverloadedStrings #-}

module JSONParsing (GamesResponse, UserGamesResponse, Game, SummaryResponse, UserSummaryResponse, Player, ownedGamesURL, gamesFromJSON, aliasURL, aliasFromJSON) where

-- Importing modules

import KEY
import IDS 

-- Importing libraries

import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit (simpleHttp)
import Data.Text hiding (map)
import Data.Aeson
import Control.Monad

-- Data types for retrieving owned games

data GamesResponse = GamesResponse
    { userGamesResponse :: UserGamesResponse
    } deriving (Show)

data UserGamesResponse = UserGamesResponse
    { gameCount :: Int
    , listOfGames :: [Game]
    } deriving (Show)

data Game = Game
    { appidGame :: Int
    , nameGame :: Text
    , playtimeForeverGame :: Int
    , imgIconURLGame :: Text
    , imgLogoURLGame :: Text
    , playtimeWindowsForeverGame :: Maybe Int
    , playtimeMACForeverGame :: Maybe Int
    , playtimeLinuxForeverGame :: Maybe Int
    , hasCommunityVisibleStatsGame :: Maybe Bool
    , playtimeMACForeveRGame :: Maybe Int
    , playtime2WeeksGame :: Maybe Int
    } deriving (Show)

-- Data types for retrieving user summaries

data SummaryResponse = SummaryResponse
    { summary :: UserSummaryResponse
    } deriving (Show)

data UserSummaryResponse = UserSummaryResponse
    { listOfPlayers :: [Player]
    } deriving (Show)

data Player = Player
    { steamidPlayer :: Text
    , communityvisibilitystatePlayer :: Int
    , profilestatePlayer :: Int
    , personanamePlayer :: Text
    , profileurlPlayer :: Text
    , avatarPlayer :: Text
    , avatarmediumPlayer :: Text
    , avatarfullPlayer :: Text
    , avatarhashPlayer :: Text
    , lastlogoffPlayer :: Int
    , personastatePlayer :: Int
    , realnamePlayer :: Maybe Text
    , primaryclanidPlayer :: Text
    , timecreatedPlayer :: Int
    , personastateflagsPlayer :: Int
    , loccountrycodePlayer :: Text
    , locstatecodePlayer :: Text
    } deriving (Show)

-- Custom FromJSON instances to parse data types for owned games

instance FromJSON GamesResponse where
    parseJSON (Object v) = GamesResponse
        <$> v .: "response"

instance FromJSON UserGamesResponse where
    parseJSON (Object v) = UserGamesResponse
        <$> v .: "game_count"
        <*> v .: "games"

instance FromJSON Game where
    parseJSON (Object v) = Game
        <$> v .: "appid"
        <*> v .: "name"
        <*> v .: "playtime_forever"
        <*> v .: "img_icon_url"
        <*> v .: "img_logo_url"
        <*> v .:? "playtime_windows_forever"
        <*> v .:? "playtime_mac_forever"
        <*> v .:? "playtime_linux_forever"
        <*> v .:? "has_community_visible_stats"
        <*> v .:? "playtime_mac_forever"
        <*> v .:? "playtime_2weeks"

-- Custom FromJSON instances to parse data types for retrieving user summaries

instance FromJSON SummaryResponse where
    parseJSON (Object v) = SummaryResponse
        <$> v .: "response"

instance FromJSON UserSummaryResponse where
    parseJSON (Object v) = UserSummaryResponse
        <$> v .: "players"

instance FromJSON Player where
    parseJSON (Object v) = Player
        <$> v .: "steamid"
        <*> v .: "communityvisibilitystate"
        <*> v .: "profilestate"
        <*> v .: "personaname"
        <*> v .: "profileurl"
        <*> v .: "avatar"
        <*> v .: "avatarmedium"
        <*> v .: "avatarfull"
        <*> v .: "avatarhash"
        <*> v .: "lastlogoff"
        <*> v .: "personastate"
        <*> v .:? "realname"
        <*> v .: "primaryclanid"
        <*> v .: "timecreated"
        <*> v .: "personastateflags"
        <*> v .: "loccountrycode"
        <*> v .: "locstatecode"

-- Pure functions        

ownedGamesURL :: String -> String
ownedGamesURL steam64 = ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steam64 ++ "&include_played_free_games=false&include_appinfo=true")

aliasURL :: String -> String
aliasURL steam64 = ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=" ++ steam64)

-- IO Functions

gamesFromJSON :: String -> IO [String]
gamesFromJSON url = do
    retrieved <- simpleHttp url
    let parsed = eitherDecode retrieved :: Either String GamesResponse
    case parsed of 
        Left error -> return [error]
        Right (GamesResponse v) ->
            let usersOwnedGames = map nameGame (listOfGames v) 
            in return (map unpack usersOwnedGames)

aliasFromJSON :: String -> IO [String]
aliasFromJSON url = do
    retrieved <- simpleHttp url
    let parsed = eitherDecode retrieved :: Either String SummaryResponse
    case parsed of 
        Left error -> return [error]
        Right (SummaryResponse v) -> 
            let alias = map personanamePlayer (listOfPlayers v)
            in return (map unpack alias)

