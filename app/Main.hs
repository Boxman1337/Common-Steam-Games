module Main where

-- Libraries
import Data.Aeson
import Network.HTTP (simpleHttp)

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
    do not change .cabal manually, each library is added automatically by stack


    References: 
    ---------------------
    https://stackoverflow.com/questions/29941866/parsing-json-data-from-a-url-in-haskell-using-aeson
    https://hackage.haskell.org/package/http-conduit-2.3.7.4/docs/Network-HTTP-Conduit.html

-}