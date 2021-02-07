-- for running in ghci, need to do ":set -package mtl" before loading this file

import Prelude hiding ((<>))
import Data.List (delete)
import Text.PrettyPrint
import Control.Monad.State          -- the State monad tracks a list of unused indices
import Control.Monad.Reader         -- the Reader monad tracks the local context
import Control.Monad.Trans.Maybe    -- the Maybe monad tracks presupposition failure

type Point = ([Int], Int)                            -- assignment sequence, world
type M a = MaybeT ((ReaderT Point) (State [Int])) a  -- our monad stack 
type Mt  = M Bool                                    -- monadic truth value
type Me  = M Int                                     -- monadic individual
type Met = M (Int -> Bool)                           -- monadic predicate

fa :: Me -> Met -> Mt    -- function application, with argument first
fa ma mf = ma >>= (\a -> mf >>= (\f -> return (f a)))

pm :: Met -> Met -> Met  -- predicate modification
pm ml mr = ml >>= (\l -> mr >>= (\r -> return (\x -> and [l x, r x])))

andP :: Mt -> Mt -> Mt
andP ml mr = ml >>= (\l -> if l then mr else return False)

notP :: Mt -> Mt
notP mp = mp >>= (\p -> return (not p))

ifP :: Mt -> Mt -> Mt
ifP ma mc = ma >>= (\a -> if a then mc else return True)

anP :: Int -> Met -> Met -> Mt
anP i r s =
  get >>= (\is -> if elem i is
                    then put (delete i is) >> (andP (fa (var i) r) (fa (var i) s))
                    else mzero)

the :: Met -> Me         -- presupposes a singleton extension
the mf = mf >>= (\f -> let ext = filter f [1..5] in
                         if length ext == 1 then return (head ext) else mzero)

poss :: (Int -> Int -> Bool) -> Me -> Me
poss rel possessor = possessor >>= (\n -> the (return (rel n)))

allowable :: Met         -- world-sensitive predicate
allowable = ask >>= (\(_,w) -> return (< w))

var :: Int -> Me         -- assigment-sensitive individual
var n = ask >>= (\(g,_) -> return (if n <= length g then g!!(n-1) else 0))
-- To do: if n is still on the list of fresh variables, should be a familiarity violation

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

everyP :: Int -> Met -> Met -> Mt
everyP i r s =
  get >>= (\is -> if elem i is
                    then put (delete i is) >>
                           foldl (\mt n -> local (\(g,w) -> (replaceAt (i-1) n g, w))
                                 (andP mt (ifP (fa (var i) r) (fa (var i) s))))
                                 (return True)
                                 [1..5]
                    else mzero)

fresh :: Int -> [Point] -> Bool  -- Heim's (22)
fresh i con = all (\(g, w) -> all (\n -> let g' = replaceAt (i-1) n g in
                                           elem (g, w) con == elem (g', w) con)
                                  [1..5])
                  con

try :: S -> [Point] -> Doc
try s c =
  let freshVars = filter (\i -> fresh i c) [1..(length c)] in
    prettyS s
    <> text ":\n" 
    <+> (text (show (filterM (\p -> evalState ((runReaderT (runMaybeT (eval s))) p) freshVars) c)))
    <> text "\n\n"

-- ------------------

data S = FA DP Pred | Succeeds DP DP | Preceeds DP DP | And S S | Not S | If S S
         | Every Int Pred Pred | A Int Pred Pred deriving (Eq, Show)
data DP = Num Int | Var Int | The Pred | SuccessorOf DP deriving (Eq, Show)
data Pred = PM Pred Pred | Allowable | Even | Odd | Prime deriving (Eq, Show)

eval :: S -> Mt
eval (FA subj pred) = fa (evalDP subj) (evalPred pred)
eval (Succeeds dp1 dp2) = evalDP dp1 >>= (\l -> evalDP dp2 >>= (\r -> return (l > r)))
eval (Preceeds dp1 dp2) = evalDP dp1 >>= (\l -> evalDP dp2 >>= (\r -> return (l < r)))
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
prettyS (Succeeds dp1 dp2) = parens $ (prettyDP dp1) <+> text "succeeds" <+> (prettyDP dp2)
prettyS (Preceeds dp1 dp2) = parens $ (prettyDP dp1) <+> text "preceeds" <+> (prettyDP dp2)
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

c0 = [([], n) | n <- [1..5]]
c1 = [([g],n) | g <- [1..5], n <- [1..5]]
c2 = [(g,n) | g <- [[v1,v2] | v1 <- [1..5], v2 <- [1..5]], n <- [1..5]]

t1 = fresh 1 c1
t2 = fresh 1 c2
t3 = fresh 2 c2

-- (3 is allowable)
s1 = try (FA (Num 3) Allowable) c0

-- ((2 is allowable) and (not (3 is allowable)))
s2 = try (And (FA (Num 2) Allowable) (Not (FA (Num 3) Allowable))) c0

-- ((2 is allowable) and ((not (3 is allowable)) and (the allowable prime is even)))
s3 = try (And (FA (Num 2) Allowable)
              (And (Not (FA (Num 3) Allowable))
                   (FA (The (PM Allowable Prime)) Even)))
         c0

-- ((the allowable prime is even) and ((not (3 is allowable)) and (2 is allowable))): Nothing
-- bear in mind that 5 does not have a successor in this tiny domain of discourse
s4 = try (And (FA (The (PM Allowable Prime)) Even)
               (And (Not (FA (Num 3) Allowable))
                    (FA (Num 2) Allowable)
         ))
         c0

-- (if (2 is allowable) (3 is allowable))
-- remember, "if" is material implication in Heim's fragment
s5 = try (If (FA (Num 2) Allowable) (FA (Num 3) Allowable)) c0

-- (if (it_1 is prime) (it_1 is even))
s6 = try (If (FA (Var 1) Prime) (FA (Var 1) Even)) c1

-- (if (3 is prime) (3 is even))
s7 = try (If (FA (Num 3) Prime) (FA (Num 3) Even)) c1

-- (every_1 prime is even)
s8 = try (Every 1 Prime Even) c1

-- (every_1 allowable prime is odd)
s9 = try (Every 1 (PM Allowable Prime) Odd) c1

-- ((not (2 is allowable)) and (every_1 allowable prime is odd))
s10 = try (And (Not (FA (Num 2) Allowable)) (Every 1 (PM Allowable Prime) Odd)) c1

-- (a_1 prime is even)
s11 = try (A 1 Prime Even) c1

-- (a_1 prime is odd)
s12 = try (A 1 Prime Odd) c1

-- ((a_1 prime is even) and (it_1's successor is odd))
s13 = try (And (A 1 Prime Even) (FA (SuccessorOf (Var 1)) Odd)) c1

-- ((a_1 prime is odd) and (it_1's successor is even))
s14 = try (And (A 1 Prime Odd) (FA (SuccessorOf (Var 1)) Even)) c1

-- ((a_1 prime is even) and (a_2 prime is odd))
s15 = try (And (A 1 Prime Even) (A 2 Prime Odd)) c2

-- (((a_1 prime is even) and (a_2 prime is odd)) and (it_2 succeeds it_1))
s16 = try (And (And (A 1 Prime Even)(A 2 Prime Odd)) (Succeeds (Var 2)(Var 1))) c2

-- (((a_1 prime is even) and (a_2 prime is odd)) and (it_2 preceeds it_1))
s17 = try (And (And (A 1 Prime Even)(A 2 Prime Odd)) (Preceeds (Var 2)(Var 1))) c2

-- (if ((a_1 prime is even) and (a_2 prime is odd)) (it_2 succeeds it_1))
-- this one is a massive fail for using material implication to approximate the conditional
s18 = try (If (And (A 1 Prime Even)(A 2 Prime Odd)) (Succeeds (Var 2)(Var 1))) c2

s19 = try (A 1 Prime Even) c0 -- as in Heim, trying to use an unfresh index is a presupposition violation

s20 = try (A 1 Prime Even) c1

main = print [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18]
