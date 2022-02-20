# CommonSteamGames

# Compile / Runtime Instructions: How to run our software
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
                    


Rules for the group project:

Where, and how often will you meet while working?
Talk via discord, a few times a week.

How do you communicate in the group - what channels? - when?
Group chat on Discord.

How will you use other tools, like trello or git?
Github for the repository.

How do you coordinate the final hand-in?
Have a final meeting where everyone confirms that their part is functional and ready to be handed in. If so, then Axel will hand in the final project file.

How do you ensure everyone knows what they should do after their current task, and what everyone else is working on?
During our discord-meetings, we will update each other on our progress and ask each other for help and check if anybody needs help.

What should you do if somebody feels left outside?
During our discord-meetings, we will ensure that nobody is left without something to work on.

What should you do if someone gets stuck on their current task?
Try and help, otherwise ask the other member if they can help.

What happens if someone feels another person isn’t doing their fair share?
If a person is seemingly not getting any work done, then you should send a message to them asking if they are stuck, or what else might cause them to be inactive. If the situation does not improve, then the TA should be contacted.

What happens if someone is not communicating in the manner or frequency agreed upon?
Send them a message on discord asking them the reason for their absence. If the situation does not improve, then the TA should be contacted.


