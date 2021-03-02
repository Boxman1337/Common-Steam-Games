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