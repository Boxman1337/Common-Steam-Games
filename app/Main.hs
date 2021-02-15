module Main where

-- Libraries
import Data.Aeson
import Network.HTTP

apiKey :: String
apiKey = "0786DE3A3F9117713096BAE4347B357A"

createURL = do
    putStrLn "Enter a valid Steam64 to a <Public> Steam Profile ..."
    steam64 <- getLine
    putStrLn ("You entered '" ++ steam64 ++ "'")

main = do
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
    
    -- Skriva en main som frågar efter steam64 och ger tillbaka en lista av ägda spel
        1) Få tillbaka en URL till Steams API från ett steam64-id

        lådan 64: 76561198068497293
        URL: http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=76561198068497293&include_played_free_games=true&include_appinfo=true
        
        2) Ansluta till URL och få tillbaka en JSON-sträng/fil
        3) Filtrera ut relevant data ur JSON-strängen
        4) Lägg data i en lista
        5) Returnera lista

    -- Jämför flera personers listor av ägda spel, filtrera bort det icke-gemensamma
        1) Skicka in flera användares steam64-id genom Steams API
        2) Få tillbaka flera listor och jämför element
        3) Returnera en ny lista med gemensamma element


    References: 
    ---------------------
    
    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html

-}