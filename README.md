# CommonSteamGames

Find out what games you and your friends have in common on Steam!

Made using Haskell

# Compile / Runtime Instructions: How to run our software
    stack build 
    
    or
    
    stack build --verbose
    
    then
    
    stack ghci ./app/Main.hs
    
    then
    
    main
    
# When installing a library, use:
    stack install <library>
    
    or 
    
    cabal install <library> 
    
    then
            
    add <library> under package.yaml dependencies (do not change .cabal manually, each library is added automatically by stack)
# Run testcases by typing these lines in terminal:
    stack ghci .\app\TestCases.hs
    
    then
    
    runtests

# Known errors and fix:
    If you get the error:
    
        "*** Exception: <stdout>: hPutChar: invalid argument (invalid character)" 
        when running our software, it is not an error from our part, but we do have a temporary fix
    
    Fix:
        :q (if you are inside the ghci environment)
        
        then
        
        chcp.com 65001 
