{-# LANGUAGE OverloadedStrings #-}

module JSONParsing (GamesResponse, UserGamesResponse, Game, SummaryResponse, UserSummaryResponse, Player, 
                    ownedGamesURL, gamesFromJSON, aliasURL, aliasFromJSON, playtimeFromJSON) where

-- Importing modules

import KEY

-- Importing libraries

import qualified Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Conduit (simpleHttp)
import Data.Text hiding (map)
import Data.Aeson

-- Data types for retrieving owned games
{- GamesResponse
   GamesResponse represents the overarching layer in the OwnedGames interface from
   Steam's API, that contains all sub-layers.
   INVARIANT: -
-}
data GamesResponse = GamesResponse
    { userGamesResponse :: UserGamesResponse
    } deriving (Show)

{-  UserGamesResponse
    UserGamesResponse represents the layer of the OwnedGames interface in Steam's API
    that contains a list of all games and the number of games.
    INVARIANT: -
-}
data UserGamesResponse = UserGamesResponse
    { gameCount :: Int
    , listOfGames :: [Game]
    } deriving (Show)

{- Game
   Game represents the data contained with a owned game in the API's OwnedGames interface.
   INVARIANT: -
-}
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
{- SummaryResponse
   SummaryResponse represents the overarching layer in the GetPlayerSummaries interface from
   Steam' API, that contains all sub-layers.
   INVARIANT: -
-}
data SummaryResponse = SummaryResponse
    { summary :: UserSummaryResponse
    } deriving (Show)

{- UserSummaryResponse
   UserSummaryResponse represents the layer containing a list of users and their data in
   the API's GetPlayerSummaries interface.
   INVARIANT: -
-}
data UserSummaryResponse = UserSummaryResponse
    { listOfPlayers :: [Player]
    } deriving (Show)

{- Player
   Player represents the data contained of a Steam user in the API's GetPlayerSummaries interface.
   INVARIANT: -
-}
data Player = Player
    { steamidPlayer :: Maybe Text
    , communityvisibilitystatePlayer :: Maybe Int
    , profilestatePlayer :: Maybe Int
    , personanamePlayer :: Text
    , profileurlPlayer :: Maybe Text
    , avatarPlayer :: Maybe Text
    , avatarmediumPlayer :: Maybe Text
    , avatarfullPlayer :: Maybe Text
    , avatarhashPlayer :: Maybe Text
    , lastlogoffPlayer :: Maybe Int
    , personastatePlayer :: Maybe Int
    , realnamePlayer :: Maybe Text
    , primaryclanidPlayer :: Maybe Text
    , timecreatedPlayer :: Maybe Int
    , personastateflagsPlayer :: Maybe Int
    , loccountrycodePlayer :: Maybe Text
    , locstatecodePlayer :: Maybe Text
    } deriving (Show)

-- All instances below is for decoding the JSON-files' contents into the corresponding custom data type(s).
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
        <$> v .:? "steamid"
        <*> v .:? "communityvisibilitystate"
        <*> v .:? "profilestate"
        <*> v .: "personaname"
        <*> v .:? "profileurl"
        <*> v .:? "avatar"
        <*> v .:? "avatarmedium"
        <*> v .:? "avatarfull"
        <*> v .:? "avatarhash"
        <*> v .:? "lastlogoff"
        <*> v .:? "personastate"
        <*> v .:? "realname"
        <*> v .:? "primaryclanid"
        <*> v .:? "timecreated"
        <*> v .:? "personastateflags"
        <*> v .:? "loccountrycode"
        <*> v .:? "locstatecode"

-- Pure functions

--Note: The API key has been replaced with "apiKey" due to security concerns.

{- ownedGamesURL id
   Creates a valid URL for Steam's API's GetOwnedGames interface.
   PRE: -
   SIDE-EFFECTS: -
   RETURNS: A valid URL for the GetOwnedGames interface containing the user's data.
   EXAMPLES:
           ownedGamesURL "76561198068497293" == "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198068497293&include_played_free_games=false&include_appinfo=true"
-}
ownedGamesURL :: String -> String
ownedGamesURL steam64 = ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steam64 ++ "&include_played_free_games=false&include_appinfo=true")

{- aliasURL id
   Creates a valid URL for Steam's API's GetPlayerSummaries interface.
   PRE: -
   SIDE-EFFECTS: -
   RETURNS: A valid URL for the GetPlayerSummaries interface containing the user's data.
   EXAMPLES:
           aliasURL "76561198068497293" == "http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198068497293"
-}
aliasURL :: String -> String
aliasURL steam64 = ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=" ++ steam64)

-- IO Functions
{- gamesFromJSON url
   Retrieves the list of owned games of a user from Steam's API.
   PRE: url must contain a valid API key and a valid Steam64 id, or else an error occurs
   SIDE-EFFECTS: 
        Sends a HTTP request to a url using simpleHttp
   RETURNS: A list containing a Steam user's owned games.   
-}

gamesFromJSON :: String -> IO [String]
gamesFromJSON url = do
    retrieved <- simpleHttp url
    let parsed = eitherDecode retrieved :: Either String GamesResponse
    case parsed of 
        Left error -> return [error]
        Right (GamesResponse v) ->
            let usersOwnedGames = map nameGame (listOfGames v) 
            in return (map unpack usersOwnedGames)
            
{- aliasFromJSON url
   Retrieves a user's username from Steam's API.
   PRE: url must contain a valid API key and a valid Steam64 id, or else an error occurs
   SIDE-EFFECTS: 
        Sends a HTTP request to a url using simpleHttp
   RETURNS: A Steam user's username.       
-}

aliasFromJSON :: String -> IO [String]
aliasFromJSON url = do
    retrieved <- simpleHttp url
    let parsed = eitherDecode retrieved :: Either String SummaryResponse
    case parsed of 
        Left error -> return [error]
        Right (SummaryResponse v) -> 
            let alias = map personanamePlayer (listOfPlayers v)
            in return (map unpack alias)

{- playtimeFromJSON url
   Retrieves a user's list of games, as well as their playtime on each game.
   PRE: url must contain a valid API key and a valid Steam64 id, or else an error occurs
   SIDE-EFFECTS: 
        Sends a HTTP request to a url using simpleHttp
   RETURNS: A list containing the owned games of a Steam user and for how long
   they have played each game.           
-}

playtimeFromJSON :: String -> IO [(String, String)]
playtimeFromJSON url = do
    retrieved <- simpleHttp url 
    let parsed = eitherDecode retrieved :: Either String GamesResponse
    case parsed of 
        Left error -> return [(error, "")]
        Right (GamesResponse v) ->
            
            -- unpack $ nameGame g = Returns the name of the game, g and unpacks it to the string data type from the text data type
            -- (show $ (playtimeForeverGame g) `div` 60) ++ " hours") = Returns the total playtime a user has played the game, g in hours

                let list = map (\g -> (unpack $ nameGame g, (show $ (playtimeForeverGame g) `div` 60) ++ " hours")) (listOfGames v)
                in return list