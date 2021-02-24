import Data.Text
import Data.List


ex = ["A Plague Tale: Innocence","Age of Empires II","ARK: Survival Evolved ", "Arma 3", "Assassin's Creed Odyssey", "BATTLETECH","BONEWORKS"]

ex1 = Prelude.drop 2 ex
ex2 = Prelude.drop 4 ex
lex3 = ex : ex1 :  ex2 : []
he = (Prelude.drop 1 lex3)

{- tallyGames list aux
   Tallies how many times a game is in a list.
   PRE: aux == []
      EXAMPLES: tallyGames (Data.List.concat lex3) [] == [("BONEWORKS",3),("BATTLETECH",3),("Assassin's Creed Odyssey",3),("Arma 3",2),("ARK: Survival Evolved ",2),("Age of Empires II",1),("A Plague Tale: Innocence",1)]
-}
tallyGames :: [String] -> [(String,Int)] -> [(String,Int)]
tallyGames [] aux = aux
tallyGames [y] aux = tallyGames [] ((y, (gamesCount y [y] 0)):aux)
tallyGames (y:ys) aux  = tallyGames (removeElement ys y) ((y,(gamesCount y (y:ys) 0)):aux)



{- removeElement list element
     Removes the ocurrences of a given element in a list.
     RETURNS: A list without the given element.
     EXAMPLES: 
	removeElement ["x","y","x","z"] "x" == ["y","z"]
        
	removeElement [] "x" == []
 -}
removeElement :: (Eq a) => [a] -> a -> [a]
-- VARIANT: length list
removeElement [] _ = []
removeElement [x] e
 | x == e = []
removeElement (x:xs) e 
 | e == x = removeElement xs e
 | otherwise = x:removeElement xs e

{- gamesCount game gameList
   Counts how many times a game is in a list.
-}
gamesCount :: String -> [String] -> Int -> Int
gamesCount x [] aux = aux
gamesCount x [y] aux
 | x == y = aux+1
 | otherwise = aux
gamesCount x (y:ys) aux
 | x == y = gamesCount x (ys) (aux+1)
 | otherwise = gamesCount x (ys) aux

