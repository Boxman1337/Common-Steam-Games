module Main where

-- Libraries
import Data.Aeson

main = do
    undefined

{- 
    Compile / Runtime Instructions:
    ---------------------

    stack build
    stack ghci ./app/Main.hs


    When installing a library, use:
    ---------------------

    stack install <library>
    then add <library> under package.yaml dependencies
    do not change .cabal manually, each library is added automatically


    References: 
    ---------------------

    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson

-}