module Main (main, inputLoop, commonGames, createTxt) where

-- Importing modules

import JSONParsing

-- Importing Libraries

import Data.List
import Data.Char
import System.IO
import GHC.IO.Encoding

-- IO Functions
{- createTxt list
   Writes all the games in list to a txt file.
   PRE: -
   RETURNS:-
   SIDE-EFFECTS: A txt file is written, containing all games in list.
   EXAMPLES:

-}
createTxt :: [String] -> IO ()
createTxt returnedList = do
    setLocaleEncoding utf8
    writeFile ("CommonGames.txt") ""
    file <- openFile "CommonGames.txt" WriteMode
    
    hPutStrLn file $ "Common Steam Games"
    hPutStrLn file $ "------------------"
    hPutStrLn file $ ""
    hPutStrLn file $ unlines returnedList
    
    hClose file


main :: IO ()
main = do 
    putStrLn ""
    putStrLn "Welcome!"
    inputLoop [] []

inputLoop :: [[String]] -> [[String]] -> IO ()
inputLoop acc acc2 = do 
    putStrLn ""
    putStrLn "Please enter a valid Steam64 to a PUBLIC Steam profile ... "

    -- Prompts the user to input a steam64

    inputID <- getLine
    let
      userURL = ownedGamesURL $ inputID
      nameURL = aliasURL $ inputID 
    
    
    -- Calling gamesFromJSON and aliasFromJSON with the steam64
    
    usergames <- gamesFromJSON userURL
    usernames <- aliasFromJSON nameURL
    let
      steamIDS = Data.List.insert usergames acc
      aliases = Data.List.insert usernames acc2

    -- Asks the user if they want to input steam64, and in that case to input the keyword 'True'

    putStrLn ""
    putStrLn "If you want to compare the following users' game libraries for common games, type 'True', otherwise type anything else. "

    confirmation <- getLine

    if ((map toUpper confirmation) == "TRUE")
        then

    -- Check if steamIDS only contain a set of owned games for one person 
    -- or if the multiple steam users dont have any games in common 

            let returnedList = commonGames steamIDS in 
                if (returnedList == []) 
                    then putStrLn "No common games were found ... "
                    else do
                          putStrLn "These are the common games for"
                          putStrLn $ getUsers (Data.List.concat aliases)
                          putStrLn "-----------------------------------------------------------------"
                          createTxt returnedList
                          putStrLn $ unlines $ returnedList
                          
                          putStrLn "-----------------------------------------------------------------"

        else
            inputLoop steamIDS aliases

-- Pure functions
test = do
  inputID <- getLine
  let
    nameURL = aliasURL $ inputID
    in do
      username <- aliasFromJSON nameURL
      putStrLn $ getUsers (username)


{- commonGames ListofLists
   Takes a list of lists ListofLists and checks for common elements within the lists inside the ListofLists
   PRE: -
   RETURNS: a list with all the common elements inside the lists within the ListofLists
   SIDE-EFFECTS: -
   EXAMPLES:
   commonGames [[1,2,3],[1],[2],[3]]     => []
   commonGames [[1,2,3],[1],[1,2],[1,3]] => [1]
   commonGames [[1,2,3]] => [1,2,3]
-}
commonGames :: Eq a => [[a]] -> [a]
commonGames [] = []
commonGames (x:(y:[])) = (Data.List.intersect x y) 
commonGames (x:[]) = x
commonGames (x:(y:ys)) = Data.List.intersect (Data.List.intersect x y) (commonGames ys)

{- getUsers list
   Takes a list containing usernames and returns them in a single string.
   PRE: -
   RETURNS: A string containing all usernames in list.
   SIDE-EFFECTS: -
   EXAMPLES:
           getUsers ["Gabe"] == "Gabe"
           getUsers ["Gabe", "Newell"] == "Gabe, Newell"
-}
getUsers :: [String] -> String
getUsers [] = ""
getUsers [x] = x
getUsers (x:xs) = x ++ ", " ++  getUsers xs

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
   
        1) Få tillbaka en URL till Steams API från ett steam64-id        
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
    
    http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=0786DE3A3F9117713096BAE4347B357A&steamid=76561198068497293&include_played_free_games=false&include_appinfo=true

    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson (Accessed 14 Feb)
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html (Accessed 15 Feb)
    https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json (Accessed 15 Feb)
    https://artyom.me/aeson (Accessed 18 Feb)
    https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString-Char8.html (Accessed 22 Feb)
    https://jsonformatter.org/json-to-haskell (Accessed 22 Feb)

-}
