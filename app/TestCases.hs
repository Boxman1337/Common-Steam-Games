module TestCases where

-- Importing modules

import KEY
import Main
import JSONParsing

-- Importing libraries

import Test.HUnit

-- Pure functions

-- getUsers
test1b = TestCase (assertEqual "for (getUsers [Gabe, Newell])"  ("Gabe, Newell") (getUsers ["Gabe", "Newell"]))
test2b = TestCase (assertEqual "for (getUsers [''])"            ("") (getUsers [""]))
test3b = TestCase (assertEqual "for (getUsers ['',''])"         (", ") (getUsers ["",""]))
test4b = TestCase (assertEqual "for (getUsers [Gabe])"          ("Gabe") (getUsers ["Gabe"]))

-- JSONParsing functions
test1j = TestCase (assertEqual "for (ownedGamesURL 76561198046588035)" ("http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=76561198046588035&include_played_free_games=false&include_appinfo=true") (ownedGamesURL "76561198046588035"))
test2j = TestCase (assertEqual "for (aliasURL 76561198046588035)" ("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=" ++ apiKey ++ "&steamids=76561198046588035") (aliasURL "76561198046588035"))

tests = TestList    [
                        TestLabel "getUsersTest1" test1b, 
                        TestLabel "getUsersTest2" test2b, 
                        TestLabel "getUsersTest3" test3b, 
                        TestLabel "getUsersTest4" test4b,
                        TestLabel "getJSONTest1" test1j,
                        TestLabel "getJSONTest2" test2j
                    ]
                    
runtests = runTestTT $ tests

