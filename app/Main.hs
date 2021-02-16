module Main where

-- Libraries
--import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Network.HTTP

data Games = Games {gameslist  :: String} 

data DataPoint = DataPoint { response :: String
                           , games :: String
                           } deriving (Show)


jsonURL = getSteam64 minSt



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


    References: 
    ---------------------
    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html

-}
