module Main (main, inputLoop, createTxt, getUsers) where

-- Importing modules

import JSONParsing
import UserPlaytime
import IDS

-- Importing Libraries

import Data.List
import Data.Char
import System.IO
import GHC.IO.Encoding
import Debug.Trace
import Test.HUnit

{- 
    Compile / Runtime Instructions: How to run our software
    ---------------------
    stack build 
    or
    stack build --verbose
        then
            stack ghci ./app/Main.hs
                then
                    main
    When installing a library, use:
    ---------------------
    stack install <library>
    or 
    cabal install <library> 
        then
            add <library> under package.yaml dependencies
            (do not change .cabal manually, each library is added automatically by stack)
    Run testcases by typing these lines in terminal:
    ------------------------------------
    stack ghci .\app\TestCases.hs
        then
            runtests
    
    Known errors and fix:
    ----------------------
    If you get the error:
        "*** Exception: <stdout>: hPutChar: invalid argument (invalid character)" 
        when running our software, it is not an error from our part, but we do have a temporary fix
    Fix:
        type: 
            :q (if you are inside the ghci environment)
                then
                    chcp.com 65001 
-}

-- IO Functions

{- main
    Starts the program.
    PRE:-
    RETURNS:-
    SIDE-EFFECTS:
        Prints to the console using putStrLn
-}

main :: IO ()
main = do 
    putStrLn "**********************************"
    putStrLn "* Welcome to Common Steam Games! *"
    putStrLn "**********************************"
    inputLoop [] []

{- inputLoop gameList userList
    Accumulates the owned games of a user and the user's name. Also prints the games and usernames.
    PRE: -
    SIDE-EFFECTS:
        Prints a message using putStrLn :: String -> IO String,
        Gets user input using getLine :: IO String
    RETURNS: The commonly owned games between the given users.
    EXAMPLES:
            Please enter a valid Steam64 to a PUBLIC Steam profile ... 
            76561198068497293
            => If you want to compare the following users' game libraries for common games, type 'True', otherwise type anything else.
            Please enter a valid Steam64 to a PUBLIC Steam profile ... 
            1
            => *** Exception: HttpExceptionRequest Request
-}

inputLoop :: [[(String, String)]] -> [[String]] -> IO ()
--VARIANT: gameList userList
inputLoop acc acc2 = do 

    setLocaleEncoding utf8

    putStrLn ""
    putStrLn "Please enter a valid Steam64 to a PUBLIC Steam profile ... "

    -- Prompts the user to input a steam64

    inputID <- getLine
    let gamesURL = ownedGamesURL $ inputID
        userName = aliasURL $ inputID 
    
    -- Calling playtimeFromJSON and aliasFromJSON with the steam64
    
    retrievedGames <- playtimeFromJSON gamesURL
    retrievedAlias <- aliasFromJSON userName

    -- Maps the user's playtime to each game in the format of "gameName, userName: x hours"

    let gamePlusPlaytime = mapAliastoPlaytime retrievedGames retrievedAlias

    -- Adds all games and playtime to the accumulator 'acc'
    -- Adds the user's alias/username to the accumulator 'acc2'

    let games = Data.List.insert gamePlusPlaytime acc
        aliases = Data.List.insert retrievedAlias acc2

    -- Asks the user if they want to input steam64, and in that case to input the keyword 'True'

    putStrLn ""
    putStrLn "If you want to compare the following users' game libraries for common games, type 'True', otherwise type anything else. "

    confirmation <- getLine

    if ((map toUpper confirmation) == "TRUE")
        then
            let returnedList = intersectPlayers games 
            in if (returnedList == []) 
                    then putStrLn "No common games were found ... "
                    else do
                            let tupleList = tupleListToString returnedList
                            putStrLn "The common games for"
                            putStrLn $ getUsers (Data.List.concat aliases) ++ " are:"
                            putStrLn "-----------------------------------------------------------------"
                            createTxt tupleList
                            putStrLn $ unlines tupleList
                            putStrLn "-----------------------------------------------------------------"

                        

        else inputLoop games aliases

{- createTxt list
    Writes all the games in list to a txt file.
    PRE: -
    RETURNS:-
    SIDE-EFFECTS: A txt file is written, containing all games in list.
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

-- Pure functions

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
--VARIANT: length list
getUsers [] = ""
getUsers [x] = x
getUsers (x:xs) = x ++ ", " ++ getUsers xs
