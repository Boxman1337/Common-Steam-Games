module UserPlaytime (mapAliastoPlaytime, intersectFirst, combinePlaytime, intersectThenMerge, intersectPlayers, tupleListToString) where

-- Importing modules

import JSONParsing

-- Importing libraries

import Data.List

-- Pure Functions

{- mapAliastoPlaytime listofTuples username
   Maps username to a users' game and playtime in the format 'username: playtime (in hours)'
   PRE: -
   RETURNS: A list of tuples [(x,y)]
   SIDE-EFFECTS: -
   EXAMPLES:
            mapAliasPlaytime [] ["Glaus"] == []
            mapAliasPlaytime [("Garry's Mod","88 hours"),("Plants vs. Zombies: Game of the Year","4 hours"),("Operation Flashpoint: Dragon Rising","47 hours"),("Aliens vs. Predator","77 hours"),("S.T.A.L.K.E.R.: Call of Pripyat","23 hours"),("Terraria","44 hours"),("Psychonauts","0 hours"),("LIMBO","0 hours"),("Amnesia: The Dark Descent","1 hours"),("Superbrothers: Sword & Sworcery EP","0 hours"),("Pox Nora","0 hours"),("Thinking with Time Machine","0 hours"),("Moonbase Alpha","0 hours"),("Gear Up","0 hours"),("Wolfenstein: The New Order","18 hours"),("Robocraft","0 hours"),("PAYDAY 2","0 hours")] ["Glaus"] == [("Garry's Mod","Glaus: 88 hours"),("Plants vs. Zombies: Game of the Year","Glaus: 4 hours"),("Operation Flashpoint: Dragon Rising","Glaus: 47 hours"),("Aliens vs. Predator","Glaus: 77 hours"),("S.T.A.L.K.E.R.: Call of Pripyat","Glaus: 23 hours"),("Terraria","Glaus: 44 hours"),("Psychonauts","Glaus: 0 hours"),("LIMBO","Glaus: 0 hours"),("Amnesia: The Dark Descent","Glaus: 1 hours"),("Superbrothers: Sword & Sworcery EP","Glaus: 0 hours"),("Pox Nora","Glaus: 0 hours"),("Thinking with Time Machine","Glaus: 0 hours"),("Moonbase Alpha","Glaus: 0 hours"),("Gear Up","Glaus: 0 hours"),("Wolfenstein: The New Order","Glaus: 18 hours"),("Robocraft","Glaus: 0 hours"),("PAYDAY 2","Glaus: 0 hours")]
-}

mapAliastoPlaytime :: [(String, String)] -> [String] -> [(String, String)]
mapAliastoPlaytime [] _ = []
mapAliastoPlaytime ((gName, gTime):xs) uName = 
    (gName, (head uName) ++ ": " ++ gTime) : mapAliastoPlaytime xs uName

intersectFirst :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersectFirst [] [] = []
intersectFirst l1 l2 = Data.List.intersectBy (\(x,y) (z,w) -> x == z) l1 l2 

combinePlaytime :: [((String, String),(String, String))] -> [(String, String)]
combinePlaytime [] = []
combinePlaytime (((x,y),(_,w)):[]) = [(x, y ++ ", " ++ w)]
combinePlaytime (((x,y),(_,w)):xs) = (x, y ++ ", " ++ w) : combinePlaytime xs

intersectThenMerge :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersectThenMerge player1 player2 = 
    let player1List = Data.List.sort $ intersectFirst player1 player2
        player2List = Data.List.sort $ intersectFirst player2 player1
    in combinePlaytime $ zip player1List player2List

intersectPlayers :: [[(String, String)]] -> [(String, String)]
intersectPlayers [] = []
intersectPlayers (x:xs) = foldl intersectThenMerge x xs

tupleListToString :: [(String, String)] -> [String]
tupleListToString [] = []
tupleListToString ((x,y):[]) = [(x ++ " -- " ++ y)]
tupleListToString ((x,y):xs) = (x ++ " -- " ++ y) : tupleListToString xs