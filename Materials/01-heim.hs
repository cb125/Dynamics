-- for running in ghci, need to do ":set -package mtl" before loading this file

import Prelude hiding ((<>))
import Control.Monad.Reader                -- the Reader monad tracks the local context
import Control.Monad.Trans.Maybe           -- the Maybe monad tracks presupposition failure
import Data.List ((\\))
import Text.PrettyPrint

type Context = [([Int], Int)]              -- list of pairs of an assignment sequence and a world
type MTv = MaybeT (Reader Context) [Bool]  -- dynamic proposition
type MInd = MaybeT (Reader Context) [Int]  -- dynamic individual
type MPred = MaybeT (Reader Context) [Int -> Bool]  -- dynamic predicate
type CCP = Maybe Context -> Maybe Context        -- context change potential

num :: Int -> MInd
num n = do
  con <- ask
  return [n | p <- con]

prop :: (Int -> Bool) -> MPred
prop p = do
  con <- ask
  return [p | point <- con]

ccp :: MTv -> CCP
ccp mprop (Just con) = do
  tvs <- runReader (runMaybeT mprop) con 
  return [point | (point, True) <- zip con tvs]
ccp _ _ = Nothing

app :: MInd -> MPred -> MTv    -- function application, with argument first
app mind mpred = do
  inds <- mind
  preds <- mpred
  return (map (\(f, a) -> f a) (zip preds inds))

pm :: MPred -> MPred -> MPred  -- predicate modification
pm ml mr = do
  l <- ml
  r <- mr
  return (map (\(f,g) -> (\x -> and [f x, g x]))
              (zip l r))

allowable :: MPred -- world-sensitive predicate
allowable = do
  con <- ask
  return [(< w) | (g, w) <- con]

the :: MPred -> MInd   -- presupposes a singleton extension
the mpred = do
  preds <- mpred
  let exts = map (\p -> filter p [1..5]) preds in
     if foldl (\bool ext -> and [bool, length ext == 1]) True exts
       then return (map head exts)
       else mzero

poss :: (Int -> Int -> Bool) -> MInd -> MInd
poss rel possessor = do
  possessors <- possessor
  let exts = map (\n -> filter (rel n) [1..5]) possessors in
    if foldl (\bool ext -> and [bool, length ext == 1]) True exts
      then return (map head exts)
      else mzero

var :: Int -> MInd     -- assigment-sensitive
var n = do
  points <- ask
  return (map (\(g,w) -> if n <= length g then g!!(n-1) else 0)
              points)

andP l r = r . l                 -- dynamic conjunction is simply function composition

notP :: CCP -> CCP
notP dp mc = do
  c1 <- mc
  c2 <- dp mc
  return (c1 \\ c2)

ifP :: CCP -> CCP -> CCP
ifP antecedent consequent con = do
  c1 <- con
  c2 <- antecedent con
  c3 <- consequent (Just c2)
  return (c1 \\ (c2 \\ c3)) -- Heim pretends "if" means material implication

replaceAt :: Int -> a -> [a] -> [a]    
replaceAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

fresh :: Int -> Context -> Bool  -- Heim's (22)
fresh i con = all (\(g, w) -> all (\n -> let g' = replaceAt (i-1) n g in
                                           elem (g, w) con == elem (g', w) con)
                                  [1..5])
                  con

everyP :: Int -> MPred -> MPred -> CCP
everyP i restriction scope con = do
    c1 <- con
    if not (fresh i c1) then mzero else do
      c2 <- ccp (app (var i) restriction) con
      c3 <- ccp (app (var i) scope) (Just c2)
      Just [(g,w) | (g,w) <- c1, all (\n -> let g' = replaceAt (i-1) n g in
                                              elem (g',w) c2 <= elem (g',w) c3) 
                                     [1..5]]

anP :: Int -> MPred -> MPred -> CCP
anP i restriction scope con = do
  c1 <- con
  if not (fresh i c1) then mzero else do
    c2 <- ccp (app (var i) restriction) con
    c3 <- ccp (app (var i) scope) (Just c2)
    return c3


-- ------------------

data S = FA DP Pred | And S S | Not S | If S S | Every Int Pred Pred | A Int Pred Pred
           deriving (Eq, Show)
data DP = Num Int | Var Int | The Pred | SuccessorOf DP | Double DP deriving (Eq, Show)
data Pred = PM Pred Pred | Allowable | Even | Odd | Prime deriving (Eq, Show)

eval :: S -> CCP
eval (FA subj pred) = ccp (app (evalDP subj) (evalPred pred))
eval (And l r) = andP (eval l) (eval r)
eval (Not p) = notP (eval p)
eval (If a c) = ifP (eval a) (eval c)
eval (Every i r s) = everyP i (evalPred r) (evalPred s)
eval (A i r s) = anP i (evalPred r) (evalPred s)

evalDP :: DP -> MInd
evalDP (Num i) = num i
evalDP (Var i) = var i
evalDP (The nom) = the (evalPred nom)
evalDP (SuccessorOf dp) = poss (\l r -> r == l + 1) (evalDP dp)

evalPred :: Pred -> MPred
evalPred Allowable = allowable
evalPred Even = prop even
evalPred Odd = prop odd
evalPred Prime = prop (\n -> elem n [2, 3, 5])
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

try :: S -> Context -> Doc
try s c = prettyS s <> text ":" <+> (text (show (eval s (Just c)))) <> text "\n"

c1 = [([], n) | n <- [1..5]]
c2 = [([g],n) | g <- [1..5], n <- [1..5]]
c3 = take (length c2 - 1) c2 -- fresh 1 c3 == False

s1 = try (FA (Num 3) Allowable) c1
s2 = try (And (Not (FA (Num 3) Allowable)) (FA (The Allowable) Odd)) c1
s3 = try (And (FA (Num 1) Allowable) (And (Not (FA (Num 2) Allowable)) (FA (The Allowable) Odd))) c1
s4 = try (If (FA (Num 2) Allowable) (FA (Num 3) Allowable)) c1 -- Heim pretends "if" means material implication
s5 = try (FA (SuccessorOf (Num 2)) Odd) c1
s6 = try (FA (SuccessorOf (Num 5)) Odd) c1
s7 = try (If (FA (Var 1) Prime) (FA (Var 1) Even)) c2
s8 = try (If (FA (Num 3) Prime) (FA (Num 3) Even)) c1
s9 = try (Every 1 Prime Even) c2
s10 = try (Every 1 (PM Allowable Prime) Odd) c2
s11 = try (Every 1 (PM Allowable Prime) Odd) c3
s12 = try (A 1 Prime Even) c2
s13 = try (A 1 Prime Odd) c2
s14 = try (And (A 1 Prime Even) (FA (SuccessorOf (Var 1)) Odd)) c2
s15 = try (And (A 1 Prime Odd) (FA (SuccessorOf (Var 1)) Even)) c2

main = print [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15]
