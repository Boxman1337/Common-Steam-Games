module TestCases where

-- Importing modules

import KEY
import Main
import JSONParsing
import UserPlaytime

-- Importing libraries

import Test.HUnit

-- Main.hs functions (getUsers)
test1b = TestCase (assertEqual "for (getUsers [Gabe, Newell])"  ("Gabe, Newell")    (getUsers ["Gabe", "Newell"]))
test2b = TestCase (assertEqual "for (getUsers [''])"            ("")                (getUsers [""]))
test3b = TestCase (assertEqual "for (getUsers ['',''])"         (", ")              (getUsers ["",""]))
test4b = TestCase (assertEqual "for (getUsers [Gabe])"          ("Gabe")            (getUsers ["Gabe"]))

-- JSONParsing functions
test1j = TestCase (assertEqual "for (ownedGamesURL 76561198046588035)"      ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true") (ownedGamesURL "76561198046588035"))
test2j = TestCase (assertEqual "for (aliasURL 76561198046588035)"           ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035") (aliasURL "76561198046588035"))

-- UserPlaytime functions
test1u = TestCase (assertEqual "for (mapAliastoPlaytime [(a, 50 hours)] [b])"                                                                         [("a", "b: 50 hours")]                                  (mapAliastoPlaytime [("a", "50 hours")] ["b"]))
test2u = TestCase (assertEqual "for (intersectFirst [(Garry's Mod, axel: 1146 hours) , (Portal,axel: 8 hours)] [(Garry's Mod, johan: 14 hours)])"     [("Garry's Mod", "axel: 1146 hours")]                   (intersectFirst [("Garry's Mod","axel: 1146 hours"),("Portal","axel: 8 hours")] [("Garry's Mod","johan: 14 hours")]))
test3u = TestCase (assertEqual "for (intersectFirst [(Garry's Mod, johan: 14 hours)] [(Garry's Mod, axel: 1146 hours),(Portal, axel: 8 hours)])"      [("Garry's Mod", "johan: 14 hours")]                    (intersectFirst [("Garry's Mod","johan: 14 hours")] [("Garry's Mod","axel: 1146 hours"),("Portal","axel: 8 hours")]))
test4u = TestCase (assertEqual "for (combinePlaytime [((Garry's Mod, johan: 50 hours),(Garry's Mod, axel: 190 hours))])"                              [("Garry's Mod", "johan: 50 hours, axel: 190 hours")]   (combinePlaytime [(("Garry's Mod","johan: 50 hours"),("Garry's Mod","axel: 190 hours"))]))
test5u = TestCase (assertEqual "for (intersectThenMerge [(Garry's Mod, axel: 21 hours)] [(Garry's Mod, johan: 17 hours)])"                            [("Garry's Mod","axel: 21 hours, johan: 17 hours")]     (intersectThenMerge [("Garry's Mod", "axel: 21 hours")] [("Garry's Mod", "johan: 17 hours")]))
test6u = TestCase (assertEqual "for (intersectPlayers [[(Garry's Mod, axel: 21 hours)],[(Garry's Mod, johan: 17 hours)]])"                            [("Garry's Mod","axel: 21 hours, johan: 17 hours")]     (intersectPlayers [[("Garry's Mod", "axel: 21 hours")],[("Garry's Mod", "johan: 17 hours")]]))
test7u = TestCase (assertEqual "for (tupleListToString [(Garry's Mod,axel: 21 hours, johan: 17 hours)])"                                              ["Garry's Mod -- axel: 21 hours, johan: 17 hours"]      (tupleListToString [("Garry's Mod","axel: 21 hours, johan: 17 hours")]))

tests = TestList    [
                        TestLabel "getUsersTest1" test1b, 
                        TestLabel "getUsersTest2" test2b, 
                        TestLabel "getUsersTest3" test3b, 
                        TestLabel "getUsersTest4" test4b,

                        TestLabel "getJSONTest1" test1j,
                        TestLabel "getJSONTest2" test2j,

                        TestLabel "mapAliatoPlaytimeTest" test1u,
                        TestLabel "intersectFirstTest1" test2u,
                        TestLabel "intersectFirstTest2" test3u,
                        TestLabel "combinePlaytimeTest" test4u,
                        TestLabel "intersectThenMergeTest" test5u,
                        TestLabel "intersectPlayersTest" test6u,
                        TestLabel "tupleListToStringTest" test7u
                    ]

runtests = runTestTT $ tests

{- 
    Run testcases by typing these lines in terminal:
    ------------------------------------

    stack ghci .\app\TestCases.hs

    then

    runtests
-}