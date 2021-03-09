-- in appendix, end of 1st paragraph, script I undefined; undefined on p. 13; terms undefined
-- back to specifying projection rules case by case
-- Heim 83 attitude towards indefinites as predicates
-- crucial for pronouns to have uniqueness presuppositions to get linearity?
-- "if" cannot be classical if we're going to have donkey anaphora

type World = Int
type Assignment = [Int]
type Context = [(Assignment, World)]
type Satt = Bool
type M a = Context -> Assignment -> World -> (Satt, Maybe a)

var :: Int -> M Int
var i c g w =
  let satt = i <= length g 
      val = if satt then Just (g!!(i-1)) else Nothing in
    (satt, val)

fa :: M Int -> M (World -> Int -> Bool) -> M Bool
fa term pred c g w =
  let satt = fst (term c g w) 
      val = snd (term c g w) >>=
        (\a -> snd (pred c g w) >>=
          (\f -> Just (f w a))) in
    (satt, val)

dand :: M Bool -> M Bool -> M Bool
dand left right c g w =
  let satt = and (map fst [left c g w, right c g w])
      val = Just (and (fmap and (map snd [left c g w, right c g w]))) in
    (satt, val)

dor :: M Bool -> M Bool -> M Bool
dor left right c g w =
  let satt = and (map fst [left c g w, right c g w])
      val = Just (or (fmap and (map snd [left c g w, right c g w]))) in
    (satt, val)

dneg :: M Bool -> M Bool
dneg prejacent c g w =
  let satt = fst (prejacent c g w)
      val = snd (prejacent c g w) >>= (\p -> Just (not p)) in
    (satt, val)

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

dex :: Int -> M Bool -> M Bool
dex i prejacent c g w =
  let satt' = any (\a -> fst (prejacent c (replaceAt i a g) w)) [1..5]
      val = Just (elem (Just True) (map (\a -> snd (prejacent c (replaceAt i a g) w)) [1..5]))
      satt = (val == Just False) || (snd (prejacent c g w) == Just True) in
    (satt, val)      
                  
devery :: Int -> M 
devery i restriction scope

lift :: a -> M a
lift a c g w = (True, Just a)

try s = map (\w -> s [] [] w) [1..5]
s1 = fa (lift 1) (lift (\w -> odd))
s2 = fa (lift 1) (lift (\w -> even))
s3 = fa (var 1) (lift (\w -> even))
s4 = fa (var 1) (lift (\w -> even)) [] [1] 2
s5 = fa (var 1) (lift (\w -> odd)) [] [1] 2
s6 = dneg (fa (var 1) (lift (\w -> odd))) [] [1] 2
s7 = dex 1 (fa (var 1) (lift (\w -> odd))) [] [1] 2
s8 = dex 1 (dand (fa (var 1) (lift (\w -> odd)))
                 (fa (var 1) (lift (\w -> even)))) [] [1] 2

-- Unintuitive resuls from reuse of an index for the indefinite
s9 = dand (dex 1 (fa (var 1) (lift (\w -> odd))))
          (dex 1 (fa (var 1) (lift (\w -> even)))) [] [1,2] 2

s10 = dand (dex 1 (fa (var 1) (lift (\w -> odd))))
           (dex 2 (fa (var 2) (lift (\w -> even)))) [] [1,2] 2

prime w i = elem i [2,3,5]

s11 = dand (dex 1 (fa (var 1) (lift (\w -> even))))
           (fa (var 1) (lift prime)) [] [4] 2

s12 = dand (dex 1 (fa (var 1) (lift (\w -> even))))
           (fa (var 1) (lift prime)) [] [2] 2
