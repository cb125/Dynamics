-- for running in ghci, need to do ":set -package mtl" before loading this file

import Prelude hiding ((<>))
import Text.PrettyPrint
import Control.Monad.Reader                -- the Reader monad tracks the local context
import Control.Monad.Trans.Maybe           -- the Maybe monad tracks presupposition failure

type Point = ([Int], Int)                      -- pair: assignment sequence, world
type Mt  = MaybeT (Reader Point) Bool          -- Monadic truth value
type Me  = MaybeT (Reader Point) Int           -- Monadic individual
type Met = MaybeT (Reader Point) (Int -> Bool) -- Monadic predicate

fa :: Me -> Met -> Mt    -- function application, with argument first
fa ma mf = ma >>= (\a -> mf >>= (\f -> return (f a)))

pm :: Met -> Met -> Met  -- predicate modification
pm ml mr = ml >>= (\l -> mr >>= (\r -> return (\x -> and [l x, r x])))

andP :: Mt -> Mt -> Mt
andP ml mr = ml >>= (\l -> if l then mr else ml)

notP :: Mt -> Mt
notP mp = mp >>= (\p -> return (not p))

ifP :: Mt -> Mt -> Mt
ifP ma mc = ma >>= (\a -> if a then mc else return True)

anP :: Int -> Met -> Met -> Mt
anP i r s = andP (fa (var i) r) (fa (var i) s)

the :: Met -> Me         -- presupposes a singleton extension
the mf = mf >>= (\f -> let ext = filter f [1..5] in
                         if length ext == 1 then return (head ext) else mzero)

poss :: (Int -> Int -> Bool) -> Me -> Me
poss rel possessor = possessor >>= (\n -> the (return (rel n)))

allowable :: Met         -- world-sensitive predicate
allowable = ask >>= (\(_,w) -> return (< w))

var :: Int -> Me         -- assigment-sensitive individual
var n = ask >>= (\(g,_) -> return (if n <= length g then g!!(n-1) else 0))

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

everyP :: Int -> Met -> Met -> Mt
everyP i r s =
  foldl (\mt n -> local (\(g,w) -> (replaceAt (i-1) n g, w))
                        (andP mt (ifP (fa (var i) r) (fa (var i) s))))
        (return True)
        [1..5]

{-
fresh :: Int -> Context -> Bool  -- Heim's (22)
fresh i con = all (\(g, w) -> all (\n -> let g' = replaceAt (i-1) n g in
                                           elem (g, w) con == elem (g', w) con)
                                  [1..5])
                  con
-}

-- ------------------

data S = FA DP Pred | And S S | Not S | If S S | Every Int Pred Pred | A Int Pred Pred
           deriving (Eq, Show)
data DP = Num Int | Var Int | The Pred | SuccessorOf DP | Double DP deriving (Eq, Show)
data Pred = PM Pred Pred | Allowable | Even | Odd | Prime deriving (Eq, Show)

eval :: S -> Mt
eval (FA subj pred) = fa (evalDP subj) (evalPred pred)
eval (And l r) = andP (eval l) (eval r)
eval (Not p) = notP (eval p)
eval (If a c) = ifP (eval a) (eval c)
eval (Every i r s) = everyP i (evalPred r) (evalPred s)
eval (A i r s) = anP i (evalPred r) (evalPred s)

evalDP :: DP -> Me
evalDP (Num i) = return i
evalDP (Var i) = var i
evalDP (The nom) = the (evalPred nom)
evalDP (SuccessorOf dp) = poss (\l r -> r == l + 1) (evalDP dp)

evalPred :: Pred -> Met
evalPred Allowable = allowable
evalPred Even = return even
evalPred Odd = return odd
evalPred Prime = return (\n -> elem n [2, 3, 5])
evalPred (PM l r) = pm (evalPred l) (evalPred r)

-- ---------------

prettyS :: S -> Doc
prettyS (FA subj pred) = parens $ (prettyDP subj) <+> text "is" <+> (prettyPred pred)
prettyS (And l r) = parens $ (prettyS l) <+> text "and" <+> (prettyS r)
prettyS (Not p) = parens $ text "not" <+> (prettyS p)
prettyS (If a c) = parens $ text "if" <+> (prettyS a) <+> (prettyS c)
prettyS (Every i r s) = parens $ text "every_" <> text (show i) <+> (prettyPred r)
                                               <+> text "is" <+> (prettyPred s)
prettyS (A i r s) = parens $ text "a_" <> text (show i) <+> (prettyPred r)
                                       <+> text "is" <+> (prettyPred s)

prettyDP :: DP -> Doc
prettyDP (Num i) = text (show i)
prettyDP (Var i) = text "it_" <> text (show i)
prettyDP (The nom) = text "the" <+> (prettyPred nom)
prettyDP (SuccessorOf dp) = prettyDP dp <> text "'s successor"

prettyPred :: Pred -> Doc
prettyPred (PM l r) = (prettyPred l) <+> (prettyPred r)
prettyPred Allowable = text "allowable"
prettyPred Even = text "even"
prettyPred Odd = text "odd"
prettyPred Prime = text "prime"

try :: S -> [Point] -> Doc
try s c = prettyS s <> text ":" <+>
          (text (show (filterM (runReader (runMaybeT (eval s))) c))) <> text "\n"

c1 = [([], n) | n <- [1..5]]
c2 = [([g],n) | g <- [1..5], n <- [1..5]]
c3 = take (length c2 - 1) c2 -- fresh 1 c3 == False

-- (3 is allowable)
--
s1 = try (FA (Num 3) Allowable) c1


-- ((not (3 is allowable)) and (the allowable is odd)): Nothing
--
s2 = try (And (Not (FA (Num 3) Allowable)) (FA (The Allowable) Odd)) c1


-- ((2 is allowable) and (not (3 is allowable)))
--
s3 = try (And (FA (Num 2) Allowable) (Not (FA (Num 3) Allowable))) c1


-- ((2 is allowable) and ((not (3 is allowable)) and (the allowable prime is even)))
--
s4 = try (And (FA (Num 2) Allowable)
              (And (Not (FA (Num 3) Allowable))
                   (FA (The (PM Allowable Prime)) Even)))
         c1


-- ((the allowable prime is even) and ((not (3 is allowable)) and (2 is allowable))): Nothing
-- bear in mind that 5 does not have a successor in this tiny domain of discourse
--
s5 = try (And (FA (The (PM Allowable Prime)) Even)
               (And (Not (FA (Num 3) Allowable))
                    (FA (Num 2) Allowable)
         ))
         c1


-- (if (2 is allowable) (3 is allowable))
--
s6 = try (If (FA (Num 2) Allowable) (FA (Num 3) Allowable)) c1


-- (if (it_1 is prime) (it_1 is even))
s7 = try (If (FA (Var 1) Prime) (FA (Var 1) Even)) c2


-- (if (3 is prime) (3 is even))
--
s8 = try (If (FA (Num 3) Prime) (FA (Num 3) Even)) c1


-- (every_1 prime is even)
--
s9 = try (Every 1 Prime Even) c2


-- (every_1 allowable prime is odd)
--
s10 = try (Every 1 (PM Allowable Prime) Odd) c2


-- (every_1 allowable prime is odd)
--
s11 = try (Every 1 (PM Allowable Prime) Odd) c3


-- ((not (2 is allowable)) and (every_1 allowable prime is odd))
--
s12 = try (And (Not (FA (Num 2) Allowable)) (Every 1 (PM Allowable Prime) Odd)) c3


-- (a_1 prime is even)
--
s13 = try (A 1 Prime Even) c2


-- (a_1 prime is odd)
--
s14 = try (A 1 Prime Odd) c2


-- ((a_1 prime is even) and (it_1's successor is odd))
--
s15 = try (And (A 1 Prime Even) (FA (SuccessorOf (Var 1)) Odd)) c2


-- ((a_1 prime is odd) and (it_1's successor is even))
--
s16 = try (And (A 1 Prime Odd) (FA (SuccessorOf (Var 1)) Even)) c2

main = print [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16]
