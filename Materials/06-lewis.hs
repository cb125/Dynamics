-- fragment from Lewis 2012; no presupposition tracking, no intentionality in predicates

import Data.List
type Assignment = [Int]

nullContext = [[x] | x <- [1..5]]

var :: Int -> Assignment -> Int
var x g = if x <= length g then g!!(x-1) else 0

lift :: (Int -> Bool) -> (Assignment -> Int) -> [Assignment]
lift fn arg = [c | c <- nullContext, fn (arg c)]

conj :: [Assignment] -> [Assignment] -> [Assignment]
conj = intersect

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

exists :: Int -> [Assignment] -> [Assignment]
exists n phi = nub [replaceAt (n-1) a k | a <- [1..5], k <- phi]

updateWithAssertion :: [Assignment] -> [Assignment] -> [Assignment]
updateWithAssertion = intersect

-- replaces each assign't in C with all ways of assigning x to an object that satisfies phi
-- this is my guess at the general statement, C[exists x phi], see fn 46
updateWithExistential :: [Assignment] -> Int -> [Assignment] -> [Assignment]
updateWithExistential context n phi =
  nub (concat (map (\g -> intersect [replaceAt (n-1) a g | a <- [1..5]] phi)
                   context))

woman n = elem n [2,3,4,5]
cameIn n = elem n [3,4,5]
lunch n = elem n [3,4]

s1 = lift woman (var 1)                 -- woman (x1)
s2 = conj s1 ((lift cameIn) (var 1))    -- came.in (x1)
s3 = exists 1 s2                        -- a^x1 woman came in
s4 = lift lunch (var 1)                 -- she_x1 ordered lunch
c1 = updateWithAssertion nullContext s3
c2 = updateWithExistential c1 1 s2
c3 = updateWithAssertion c2 s4

{-
32. a. A^x woman walked in.
    b. She_1 ordered lunch. 

Let’s see how PL?D explains (32). Conversational participants always
update the context with the informational content of an assertion (by
intersection). The content of (32a) is the set of assignment functions
such that for each assignment function, there exists at least one
x-variant that assigns x to a woman who walked in. If there is such an
object in the model, this will amount to the denotation being the set
of all assignment functions. The intersective update, therefore, has
no effect on the input context.

nullContext == [[1],[2],[3],[4],[5]]
c1 == [[1],[2],[3],[4],[5]]

(32a), as an existential, also triggers the second pragmatic
update. The function for adding a new file card takes the input
context and returns all the x-variants of each assignment function
that assign x to a woman who walked in. So after semantically and
pragmatically processing (32a), the context is in the same state as
after the semantic processing of the same sentence according to
DPL.

c2 == [[3],[4],[5]]

(32b) will trigger the normal intersective update. Its content is
the set of assignment functions that assign x to something that
ordered lunch. Since all the the assignment functions in its input
context assign x to a woman who walked in, the resulting output
context will include only assignments that assign x to a woman who
walked in and ordered lunch.

c3 == [[3],[4]]

Same example run through the DPL evaluator from week 2:

-- Lewis 2012 example
woman = Precedes (Num 1)    -- 2,3,4,5
cameIn = Precedes (Num 2)   -- 3,4,5
lunch = Follows (Num 5)     -- 1,2,3,4
c13 = evalFor (Exists 1 (And (woman (Var 1)) (cameIn (Var 1)))) c1
-- c13 == [[3],[4],[5]]
c14 = evalFor (lunch (Var 1)) c13
-- c14 == [[3],[4]]

-}
