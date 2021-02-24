{-#LANGUAGE DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Libraries
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import Network.HTTP.Conduit
import GHC.Generics
import Data.Text
import Data.List


data User =
    User { username :: String
         , steam64 :: String
         , ownedGames :: [String]
         } deriving (Show)


data JSONResponse = JSONResponse
    { userResponse :: UserResponse
    } deriving (Show)


data UserResponse = UserResponse
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



instance FromJSON JSONResponse where
    parseJSON (Object v) = JSONResponse
        <$> v .: "response"


instance FromJSON UserResponse where
    parseJSON (Object v) = UserResponse
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
        <*> v .:? "playtime_mac_foreve r"
        <*> v .:? "playtime_2weeks"





jsonURL :: String
jsonURL = getSteam64 minSt

--getJSON :: IO B.ByteString
--getJSON = simpleHTTP $ getRequest jsonURL

getJSON2 :: IO B.ByteString
getJSON2 = simpleHttp jsonURL

minSt = "76561198046588035"

--dec :: Maybe Value
--dec = decode <$> getJSON2

tst :: IO ()
tst = do
   jss <- decode <$> getJSON2
   print (jss :: Maybe Value)



mainj = do
    putStrLn "Enter a steam64 id:"
    stId <- getLine
    if stId == "no" then do
      mainj
        else do
      getJSON2

getSteam64  :: String -> String
getSteam64 "" =  ""
getSteam64 s = "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=" ++ s  ++  "&include_played_free_games=true&include_appinfo=true"



apiKey :: String
apiKey = "0786DE3A3F9117713096BAE4347B357A"

stdURL :: String
stdURL = "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=76561198068497293&include_played_free_games=false&include_appinfo=true"

createURL = do
    putStrLn "Enter a valid Steam64 to a public Steam Profile ..."
    steam64 <- getLine
    putStrLn ("You entered '" ++ steam64 ++ "'")
    return ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steam64 ++ "&include_played_free_games=false&include_appinfo=true")

getJSON :: IO C.ByteString
getJSON = simpleHttp stdURL


main = do
    jsonFormat <- simpleHttp stdURL
    let parsed = decode jsonFormat :: Maybe JSONResponse
    case parsed of 
        Just value -> case value of
            JSONResponse value -> return $ listOfGames value


{- 
    Compile / Runtime Instructions:
    ---------------------

    stack build
    stack ghci ./app/Main.hs


    When installing a library, use:
    ---------------------
    
    stack install <library>
    or 
    cabal install <library>
    then add <library> under package.yaml dependencies
    do not change .cabal manually, each library is added automatically by stack


    To-do (Divide and Conquer)
    ---------------------
    
        Skriva en main som frågar efter steam64 och ger tillbaka en lista av ägda spel
        1) Få tillbaka en URL till Steams API från ett steam64-id

        L�dan 64: 76561198068497293
        URL: http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=76561198068497293&include_played_free_games=false&include_appinfo=true
        
        2) Ansluta till URL och få tillbaka en JSON-sträng/fil
        3) Filtrera ut relevant data ur JSON-strängen
        4) Lägg data i en lista
        5) Returnera lista

        Jämför flera personers listor av ägda spel, filtrera bort det icke-gemensamma
        1) Skicka in flera användares steam64-id genom Steams API
        2) Få tillbaka flera listor och jämför element
        3) Returnera en ny lista med gemensamma element


    References: 
    ---------------------
    
    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson (Accessed 14 Feb)
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html (Accessed 15 Feb)
    https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json (Accessed 15 Feb)
    https://artyom.me/aeson (Accessed 18 Feb)
    https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString-Char8.html (Accessed 22 Feb)

-}

