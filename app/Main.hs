module Main where

-- Importing modules

import JSONParsing

-- Importing Libraries

import Data.List

main = do
    putStrLn "Welcome! Please enter a valid Steam64 to a PUBLIC Steam profile ... "
    inputID <- getLine
    let userURL = createURL $ inputID
    putStrLn ""
    putStrLn ("Calling extractFromJSON with URL: " ++ userURL)
    putStrLn ""
    usergames <- returnFromJSON userURL
    let pureList = usergames
    print pureList
    putStrLn ""
    putStrLn "If you want to compare the following lists, type 'True' "
    putStrLn ""
    confirmation <- getLine
    if confirmation == "True"
        then
            print  (lilintoc pureList)
        else
            test ([pureList])

test acc = do 
    putStrLn "Please enter a valid Steam64 to a PUBLIC Steam profile ... "
    inputID <- getLine
    let userURL = createURL $ inputID
    putStrLn ""
    putStrLn ("Calling extractFromJSON with URL: " ++ userURL)
    putStrLn ""
    usergames <- returnFromJSON userURL
    let all = Data.List.insert usergames acc
    print all
    putStrLn ""
    putStrLn "If you want to compare the following lists, type 'True' "
    putStrLn ""
    confirmation <- getLine
    if confirmation == "True"
        then
            print  (lilintoc (all))
        else
            test all


lilintoc [] = []
lilintoc (x:(y:[])) = (Data.List.intersect x y) 
lilintoc (x:[]) = x
lilintoc (x:(y:ys)) = Data.List.intersect (Data.List.intersect x y) (lilintoc ys)