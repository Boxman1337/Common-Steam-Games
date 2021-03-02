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
   PRE: id must be a valid Steam64 ID. An available String called apiKey that contains a valid key for Steam's API.
   SIDE-EFFECTS: -
   RETURNS: A valid URL for the GetOwnedGames interface containing the user's data.
   EXAMPLES:
           ownedGamesURL "76561198068497293" == "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198068497293&include_played_free_games=false&include_appinfo=true"
-}
ownedGamesURL :: String -> String
ownedGamesURL steam64 = ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steam64 ++ "&include_played_free_games=false&include_appinfo=true")

{- aliasURL id
   Creates a valid URL for Steam's API's GetPlayerSummaries interface.
   PRE: id must be a valid Steam64 ID. An available String called apiKey that contains a valid key for Steam's API.
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
   PRE: url must contain a valid API key and a valid Steam64 id.
   SIDE-EFFECTS: -
   RETURNS: A list containing a Steam user's owned games.
   EXAMPLES:
           gamesFromJSON ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198030522647&include_played_free_games=false&include_appinfo=true") == ["Garry's Mod","Plants vs. Zombies: Game of the Year","Operation Flashpoint: Dragon Rising","Aliens vs. Predator","S.T.A.L.K.E.R.: Call of Pripyat","Terraria","Psychonauts","LIMBO","Amnesia: The Dark Descent","Superbrothers: Sword & Sworcery EP","Pox Nora","Thinking with Time Machine","Moonbase Alpha","Gear Up","Wolfenstein: The New Order","Robocraft","PAYDAY 2"]

           gamesFromJSON ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198030522647&include_played_free_games=false&include_appinfo=true") == ["Garry's Mod","Plants vs. Zombies: Game of the Year","Operation Flashpoint: Dragon Rising","Aliens vs. Predator","S.T.A.L.K.E.R.: Call of Pripyat","Terraria","Psychonauts","LIMBO","Amnesia: The Dark Descent","Superbrothers: Sword & Sworcery EP","Pox Nora","Thinking with Time Machine","Moonbase Alpha","Gear Up","Wolfenstein: The New Order","Robocraft","PAYDAY 2"]
   
   
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
   PRE: url must contain a valid API key and a valid Steam64 id.
   SIDE-EFFECTS: -
   RETURNS: A Steam user's username.
   EXAMPLES:
           aliasFromJSON ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198030522647") == ["the"]
           aliasFromJSON ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035") == ["Glaus"]
           
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
   PRE: url must contain a valid API key and a valid Steam64 id.
   SIDE-EFFECTS: -
   RETURNS: A list containing the owned games of a Steam user and for how long
   they have played each game.
   EXAMPLES:
           playtimeFromJSON  ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true") ==
[("Counter-Strike","0 hours"),("Counter-Strike: Condition Zero","0 hours"),("Counter-Strike: Condition Zero Deleted Scenes","0 hours"),("Day of Defeat: Source","0 hours"),("Team Fortress Classic","0 hours"),("Day of Defeat","0 hours"),("Deathmatch Classic","0 hours"),("Half-Life: Opposing Force","0 hours"),("Ricochet","0 hours"),("Half-Life","0 hours"),("Half-Life: Blue Shift","0 hours"),("Half-Life 2","0 hours"),("Half-Life 2: Lost Coast","0 hours"),("Counter-Strike: Source","0 hours"),("Half-Life: Source","0 hours"),("Half-Life Deathmatch: Source","0 hours"),("Half-Life 2: Deathmatch","0 hours"),("Half-Life 2: Episode One","0 hours"),("Garry's Mod","49 hours"),("Portal","55 hours"),("Half-Life 2: Episode Two","0 hours"),("Left 4 Dead","0 hours"),("Star Wars: Battlefront 2 (Classic, 2005)","22 hours"),("LEGO\9415 Indiana Jones\8482: The Original Adventures","9 hours"),("LEGO\174 Star Wars\8482: The Complete Saga","0 hours"),("Left 4 Dead 2","1 hours"),("Portal 2","44 hours"),("Crusader Kings II","29 hours"),("Counter-Strike: Global Offensive","821 hours"),("War Thunder","11 hours"),("Thinking with Time Machine","1 hours"),("Moonbase Alpha","0 hours"),("No More Room in Hell","0 hours"),("EVGA Precision X1","4 hours"),("Sven Co-op","0 hours"),("Robocraft","41 hours"),("Everlasting Summer","0 hours"),("NEKOPARA Vol. 1","2 hours"),("ARK: Survival Evolved","82 hours"),("ARK: Survival Of The Fittest","0 hours"),("Brawlhalla","0 hours"),("Undertale","19 hours"),("Danganronpa: Trigger Happy Havoc","6 hours"),("Creativerse","0 hours"),("DOOM","0 hours"),("Risk of Rain 2","15 hours"),("PAYDAY 2","0 hours"),("Sonic Mania","4 hours"),("Doki Doki Literature Club","0 hours"),("Danganronpa V3: Killing Harmony","49 hours"),("Phoenix Wright: Ace Attorney Trilogy","24 hours"),("Among Us","17 hours"),("200% Mixed Juice!","0 hours"),("Acceleration of SUGURI 2","0 hours"),("100% Orange Juice","1 hours"),("Gang of Four","0 hours"),("Helltaker","1 hours"),("Total War: SHOGUN 2","0 hours"),("The LEGO\174 NINJAGO\174 Movie Video Game","0 hours"),("Unfortunate Spacemen","0 hours"),("SEGA Mega Drive & Genesis Classics","5 hours"),("Ultimate Epic Battle Simulator","0 hours")]
           
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

            


-- TEST CASES
{-
test1 = TestCase (assertEqual "for (ownedGamesURL "76561198046588035")" "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true" (ownedGamesURL "76561198046588035"))

test2 = TestCase (assertEqual "for (aliasURL "76561198046588035")" "http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035"" (aliasURL "76561198046588035"))

test3 = TestCase (assertEqual "for gamesFromJSON  ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true")") (["Counter-Strike","Counter-Strike: Condition Zero","Counter-Strike: Condition Zero Deleted Scenes","Day of Defeat: Source","Team Fortress Classic","Day of Defeat","Deathmatch Classic","Half-Life: Opposing Force","Ricochet","Half-Life","Half-Life: Blue Shift","Half-Life 2","Half-Life 2: Lost Coast","Counter-Strike: Source","Half-Life: Source","Half-Life Deathmatch: Source","Half-Life 2: Deathmatch","Half-Life 2: Episode One","Garry's Mod","Portal","Half-Life 2: Episode Two","Left 4 Dead","Star Wars: Battlefront 2 (Classic, 2005)","LEGO\9415 Indiana Jones\8482: The Original Adventures","LEGO\174 Star Wars\8482: The Complete Saga","Left 4 Dead 2","Portal 2","Crusader Kings II","Counter-Strike: Global Offensive","War Thunder","Thinking with Time Machine","Moonbase Alpha","No More Room in Hell","EVGA Precision X1","Sven Co-op","Robocraft","Everlasting Summer","NEKOPARA Vol. 1","ARK: Survival Evolved","ARK: Survival Of The Fittest","Brawlhalla","Undertale","Danganronpa: Trigger Happy Havoc","Creativerse","DOOM","Risk of Rain 2","PAYDAY 2","Sonic Mania","Doki Doki Literature Club","Danganronpa V3: Killing Harmony","Phoenix Wright: Ace Attorney Trilogy","Among Us","200% Mixed Juice!","Acceleration of SUGURI 2","100% Orange Juice","Gang of Four","Helltaker","Total War: SHOGUN 2","The LEGO\174 NINJAGO\174 Movie Video Game","Unfortunate Spacemen","SEGA Mega Drive & Genesis Classics","Ultimate Epic Battle Simulator"] :: IO [String]) (gamesFromJSON  "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true")

test4 =  TestCase (assertEqual "for playtimeFromJSON  ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true")") ([("Counter-Strike","0 hours"),("Counter-Strike: Condition Zero","0 hours"),("Counter-Strike: Condition Zero Deleted Scenes","0 hours"),("Day of Defeat: Source","0 hours"),("Team Fortress Classic","0 hours"),("Day of Defeat","0 hours"),("Deathmatch Classic","0 hours"),("Half-Life: Opposing Force","0 hours"),("Ricochet","0 hours"),("Half-Life","0 hours"),("Half-Life: Blue Shift","0 hours"),("Half-Life 2","0 hours"),("Half-Life 2: Lost Coast","0 hours"),("Counter-Strike: Source","0 hours"),("Half-Life: Source","0 hours"),("Half-Life Deathmatch: Source","0 hours"),("Half-Life 2: Deathmatch","0 hours"),("Half-Life 2: Episode One","0 hours"),("Garry's Mod","49 hours"),("Portal","55 hours"),("Half-Life 2: Episode Two","0 hours"),("Left 4 Dead","0 hours"),("Star Wars: Battlefront 2 (Classic, 2005)","22 hours"),("LEGO\9415 Indiana Jones\8482: The Original Adventures","9 hours"),("LEGO\174 Star Wars\8482: The Complete Saga","0 hours"),("Left 4 Dead 2","1 hours"),("Portal 2","44 hours"),("Crusader Kings II","29 hours"),("Counter-Strike: Global Offensive","821 hours"),("War Thunder","11 hours"),("Thinking with Time Machine","1 hours"),("Moonbase Alpha","0 hours"),("No More Room in Hell","0 hours"),("EVGA Precision X1","4 hours"),("Sven Co-op","0 hours"),("Robocraft","41 hours"),("Everlasting Summer","0 hours"),("NEKOPARA Vol. 1","2 hours"),("ARK: Survival Evolved","82 hours"),("ARK: Survival Of The Fittest","0 hours"),("Brawlhalla","0 hours"),("Undertale","19 hours"),("Danganronpa: Trigger Happy Havoc","6 hours"),("Creativerse","0 hours"),("DOOM","0 hours"),("Risk of Rain 2","15 hours"),("PAYDAY 2","0 hours"),("Sonic Mania","4 hours"),("Doki Doki Literature Club","0 hours"),("Danganronpa V3: Killing Harmony","49 hours"),("Phoenix Wright: Ace Attorney Trilogy","24 hours"),("Among Us","17 hours"),("200% Mixed Juice!","0 hours"),("Acceleration of SUGURI 2","0 hours"),("100% Orange Juice","1 hours"),("Gang of Four","0 hours"),("Helltaker","1 hours"),("Total War: SHOGUN 2","0 hours"),("The LEGO\174 NINJAGO\174 Movie Video Game","0 hours"),("Unfortunate Spacemen","0 hours"),("SEGA Mega Drive & Genesis Classics","5 hours"),("Ultimate Epic Battle Simulator","0 hours")] :: IO [(String, String)]) (playtimeFromJSON  ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true"))


test5 = TestCase (assertEqual "for aliasFromJSON ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035")," ["Glaus"] (aliasFromJSON ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035"))


-}
