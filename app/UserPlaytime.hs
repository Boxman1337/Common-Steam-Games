module UserPlaytime (mapAliastoPlaytime, intersectFirst, combinePlaytime, intersectThenMerge, intersectPlayers, tupleListToString) where

-- Importing modules

import JSONParsing

-- Importing libraries

import Data.List

-- Pure Functions

{- mapAliastoPlaytime listofTuples username
    Maps username to a users' game and playtime in the format 'username: playtime (in hours)
    PRE: -
    RETURNS: A list of tuples [(x,y)] where x and y are strings (x = game name, y = playtime)
    SIDE-EFFECTS: -
    EXAMPLES: 
            mapAliastoPlaytime [("Garry's Mod", "50 hours")] ["axel"] == [("Garry's Mod", "axel: 50 hours")]
            mapAliasPlaytime [] ["Glaus"] == []
            mapAliasPlaytime [("Garry's Mod","88 hours"),("Plants vs. Zombies: Game of the Year","4 hours"),("Operation Flashpoint: Dragon Rising","47 hours"),("Aliens vs. Predator","77 hours"),("S.T.A.L.K.E.R.: Call of Pripyat","23 hours"),("Terraria","44 hours"),("Psychonauts","0 hours"),("LIMBO","0 hours"),("Amnesia: The Dark Descent","1 hours"),("Superbrothers: Sword & Sworcery EP","0 hours"),("Pox Nora","0 hours"),("Thinking with Time Machine","0 hours"),("Moonbase Alpha","0 hours"),("Gear Up","0 hours"),("Wolfenstein: The New Order","18 hours"),("Robocraft","0 hours"),("PAYDAY 2","0 hours")]            ["Glaus"] == [("Garry's Mod","Glaus: 88 hours"),("Plants vs. Zombies: Game of the Year","Glaus: 4 hours"),("Operation Flashpoint: Dragon Rising","Glaus: 47 hours"),("Aliens vs. Predator","Glaus: 77 hours"),("S.T.A.L.K.E.R.: Call of Pripyat","Glaus: 23 hours"),("Terraria","Glaus: 44 hours"),("Psychonauts","Glaus: 0 hours"),("LIMBO","Glaus: 0 hours"),("Amnesia: The Dark Descent","Glaus: 1 hours"),("Superbrothers: Sword & Sworcery EP","Glaus: 0 hours"),("Pox Nora","Glaus: 0 hours"),("Thinking with Time Machine","Glaus: 0 hours"),("Moonbase Alpha","Glaus: 0 hours"),("Gear Up","Glaus: 0 hours"),("Wolfenstein: The New Order","Glaus: 18 hours"),("Robocraft","Glaus: 0 hours"),("PAYDAY 2","Glaus: 0 hours")]
-}

mapAliastoPlaytime :: [(String, String)] -> [String] -> [(String, String)]
--VARIANT: length listofTuples
mapAliastoPlaytime [] _ = []
mapAliastoPlaytime ((gName, gTime):xs) uName = 
    (gName, (head uName) ++ ": " ++ gTime) : mapAliastoPlaytime xs uName

{- intersectFirst l1 l2
    Compares two lists' first element inside each tuple
    PRE: -
    RETURNS: A list of tuples [(x,y)] where x and y are strings (x = game name, y = username and playtime for the user representing l1)
    SIDE-EFFECTS: -
    EXAMPLES: 
            intersectFirst [("Garry's Mod","axel: 1146 hours"),("Portal","axel: 8 hours")] [("Garry's Mod","johan: 14 hours")] == [("Garry's Mod", "axel: 1146 hours")]
            intersectFirst [("Garry's Mod","johan: 14 hours")] [("Garry's Mod","axel: 1146 hours"),("Portal","axel: 8 hours")] == [("Garry's Mod", "johan: 14 hours")]
-}          

intersectFirst :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersectFirst [] [] = []
intersectFirst l1 l2 = Data.List.intersectBy (\(x,y) (z,w) -> x == z) l1 l2 

{- combinePlaytime list
    Combines two lists containing tuples of game names and the two users' usernames and playtimes for each game into one (concatenates the usernames and playtimes)
    PRE: -
    RETURNS: A list of tuples [(x,y)] where x and y are strings (x = game name, y = "user1: h hours, user2: k hours")
    SIDE-EFFECTS: -
    EXAMPLES: 
            combinePlaytime [(("Garry's Mod","johan: 50 hours"),("Garry's Mod","axel: 190 hours"))] == [("Garry's Mod", "johan: 50 hours, axel: 190 hours")]
-}          

combinePlaytime :: [((String, String),(String, String))] -> [(String, String)]
--VARIANT: length list
combinePlaytime [] = []
combinePlaytime (((x,y),(_,w)):[]) = [(x, y ++ ", " ++ w)]
combinePlaytime (((x,y),(_,w)):xs) = (x, y ++ ", " ++ w) : combinePlaytime xs

{- intersectThenMerge l1 l2
    Takes two lists, retrieves the lists of elements (games) they have in common with regard to each users' playtime,
    and then merges these by first zipping the lists to get the lists in the format of [(("a","x: z hours"),("a","y: w hours"))],
    and then use that list as an argument in combinePlaytime
    PRE: -
    RETURNS: A list of tuples [(x,y)] where x and y are strings (x = game name, y = "user1: h hours, user2: k hours")
    SIDE-EFFECTS: -
    EXAMPLES: 
            intersectThenMerge [("Garry's Mod", "axel: 21 hours")] [("Garry's Mod", "johan: 17 hours")] == [("Garry's Mod","axel: 21 hours, johan: 17 hours")]
-}         

intersectThenMerge :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersectThenMerge player1 player2 = 
    let player1List = Data.List.sort $ intersectFirst player1 player2
        player2List = Data.List.sort $ intersectFirst player2 player1
    in combinePlaytime $ zip player1List player2List

{- intersectPlayers list
    Takes list, filled with games and each users' respective playtime from multiple users, 
    and then passes these through the intersectThenMerge to get a single list of tuples with each game and each users' playtime on said game
    PRE: -
    RETURNS: A list of tuples [(x,y)] where x and y are strings (x = game name, y = "user1: h hours, user2: k hours")
    SIDE-EFFECTS: -
    EXAMPLES: 
            intersectPlayers [[("Garry's Mod", "axel: 21 hours")],[("Garry's Mod", "johan: 17 hours")]] == [("Garry's Mod","axel: 21 hours, johan: 17 hours")] 
-}         

intersectPlayers :: [[(String, String)]] -> [(String, String)]
intersectPlayers [] = []
intersectPlayers (x:xs) = foldl intersectThenMerge x xs

{- tupleListToString list
    Converts each element of list into a string
    PRE: -
    RETURNS: A list of strings 
    SIDE-EFFECTS: -
    EXAMPLES: 
            tupleListToString [("Garry's Mod","axel: 21 hours, johan: 17 hours")] == ["Garry's Mod -- axel: 21 hours, johan: 17 hours"]
-}      

tupleListToString :: [(String, String)] -> [String]
--VARIANT: length list
tupleListToString [] = []
tupleListToString ((x,y):[]) = [(x ++ " -- " ++ y)]
tupleListToString ((x,y):xs) = (x ++ " -- " ++ y) : tupleListToString xs
