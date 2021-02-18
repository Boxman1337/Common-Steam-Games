{-#LANGUAGE DeriveGeneric#-}


module Main where

-- Libraries
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Network.HTTP
import Control.Applicative {- os�ker-}
import Control.Monad
import GHC.Generics

data User =
    User { username :: String
         , steam64 :: String
         , ownedGames :: [String]
         } deriving (Show,Generic)





instance FromJSON User
instance ToJSON User


jsonURL :: String
jsonURL = getSteam64 minSt

--getJSON :: IO B.ByteString
--getJSON = simpleHTTP $ getRequest jsonURL

getJSON2 :: IO B.ByteString
getJSON2 = simpleHttp jsonURL

minSt = "76561198046588035"






main = do
    putStrLn "Enter a steam64 id:"
    stId <- getLine
    if stId == "no"
      then do
        main
        else putStrLn (getSteam64 stId)

getSteam64  :: String -> String
getSteam64 "" =  ""
getSteam64 s = "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=" ++ s  ++  "&include_played_free_games=true&include_appinfo=true"


apiKey :: String
apiKey = "0786DE3A3F9117713096BAE4347B357A"

createURL = do
    putStrLn "Enter a valid Steam64 to a public Steam Profile ..."
    steam64 <- getLine
    putStrLn ("You entered '" ++ steam64 ++ "'")
    putStrLn ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steam64 ++ "&include_played_free_games=false&include_appinfo=true")

main3 = do
    createURL


{- 
    Compile / Runtime Instructions:
    ---------------------

    stack build
    stack ghci ./app/Main.hs


    When installing a library, use:
    ---------------------
    
    stack install <library>
    then add <library> under package.yaml dependencies
    do not change .cabal manually, each library is added automatically by stack


    To-do (Divide and Conquer)
    ---------------------
    
        Skriva en main som frågar efter steam64 och ger tillbaka en lista av ägda spel
        1) Få tillbaka en URL till Steams API från ett steam64-id

        lådan 64: 76561198068497293
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
    
    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html
    https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json

-}
