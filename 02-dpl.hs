-- for running in ghci, need to do ":set -package mtl" before loading this file
import Data.List (nub)

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take (i-1) xs ++ [x] ++ drop i xs

data Term = Num Int | Var Int deriving (Eq, Show)
data For = Even Term | Odd Term | Prime Term | Precedes Term Term | Follows Term Term | Equals Term Term |
           Not For | And For For | Or For For | If For For | Exists Int For | Forall Int For
           deriving (Eq, Show)

evalTerm :: Term -> [Int] -> Int
evalTerm (Num n) _ = n
evalTerm (Var i) g = if i <= length g then g!!(i-1) else 0

evalFor :: For -> [[Int] -> [Int]]
-- the values that begin with "filter" are what G&S call "tests"
evalFor (Even t) gs = filter (\g -> even (evalTerm t g)) gs
evalFor (Odd t) gs = filter (\g -> odd (evalTerm t g)) gs
evalFor (Prime t) gs = filter (\g -> elem (evalTerm t g) [2,3,5]) gs
evalFor (Precedes t1 t2) gs = filter (\g -> (evalTerm t1 g) < (evalTerm t2 g)) gs
evalFor (Follows t1 t2) gs = filter (\g -> (evalTerm t1 g) > (evalTerm t2 g)) gs
evalFor (Equals t1 t2) gs = filter (\g -> (evalTerm t1 g) == (evalTerm t2 g)) gs
evalFor (Not s) gs = filter (\g -> [] == evalFor s [g]) gs
evalFor (And s1 s2) gs = evalFor s2 (evalFor s1 gs)
evalFor (Or s1 s2) gs = filter (\g -> elem g ((evalFor s1 gs) ++ (evalFor s2 gs))) gs
evalFor (If s1 s2) gs = filter (\g -> all (\k -> [] /= (evalFor s2 [k])) (evalFor s1 [g])) gs
evalFor (Exists i s) gs = evalFor s (nub (concat (map (\g -> map (\n -> replaceAt i n g) [1..5]) gs)))
evalFor (Forall i s) gs = filter (\g -> all (\k -> [] /= (evalFor s [k]))
                                            (map (\n -> replaceAt i n g) [1..5])) gs

c1 = [[1],[2],[3],[4],[5]]

s1 = evalFor (Even (Var 1)) c1
s2 = evalFor (Exists 1 (Odd (Var 1))) c1
s3 = evalFor (Exists 1 (Not (Odd (Var 1)))) c1
s4 = evalFor (And (Even (Num 2)) (Odd (Num 3))) c1
s5 = evalFor (Exists 1 (And (Even (Var 1)) (Prime (Var 1)))) c1
s6 = evalFor (Exists 1 (Or (Even (Var 1)) (Prime (Var 1)))) c1
s7 = evalFor (If (And (Even (Var 1)) (Prime (Var 1))) (Equals (Num 2) (Var 1))) c1
s8 = evalFor (Forall 1 (If (Odd (Var 1)) (Prime (Var 1)))) c1
s9 = evalFor (Forall 1 (If (And (Precedes (Var 1) (Num 3)) (Even (Var 1)))
                           (Prime (Var 1)))) c1

-- Donkey anaphora
s10 = evalFor (If (Exists 1 (And (Even (Var 1)) (Prime (Var 1)))) (Equals (Num 2) (Var 1))) c1
s11 = evalFor (Exists 1 (And (Even (Var 1)) (Exists 2 (And (Odd (Var 2)) (Precedes (Var 1) (Var 2))))))
              [[0,0]]
s12 = evalFor (If (Exists 1 (And (Even (Var 1)) (Exists 2 (And (Odd (Var 2)) (Precedes (Var 1) (Var 2))))))
                  (Follows (Var 2) (Var 1)))
              [[0,0]]
