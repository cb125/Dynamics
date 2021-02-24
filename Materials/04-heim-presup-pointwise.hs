-- Heim 1983, presup only, pointwise
--   presup projection implemented as lazy logical operators which are pure wrt presup failure

import Control.Monad.Reader 
import Control.Monad.Trans.Maybe

type M a = MaybeT (Reader Int) a

mand, mor, mif :: M Bool -> M Bool -> M Bool
mand ml mr = ml >>= (\l -> if l then mr else return False)
mor ml mr = ml >>= (\l -> if l then return True else mr)
mif ml mr = ml >>= (\l -> if l then mr else return True)

mnot :: M Bool -> M Bool
mnot ma = ma >>= (\l -> return (not l))

fa a f = f a            -- backwards function application
pm l r x = l x && (r x) -- predicate modification

mallowable :: M (Int -> Bool)
mallowable = ask >>= (\w -> return (\i -> i < w))

the :: M (Int -> Bool) -> M Int
the mr = do
  r <- mr
  let ext = filter r [1..5] in
    if length ext == 1          -- presupposes singleton extension
      then return (head ext)
      else mzero

run :: M Bool -> [Maybe Bool]
run s = map (\w -> runReader (runMaybeT s) w) [1..5]

s1 = liftM fa (return 1) <*> mallowable 
-- 1 is allowable
-- run s1 ~~> [Just False,Just True,Just True,Just True,Just True]

s2 = mnot (liftM fa (return 2) <*> mallowable)
-- 2 is not allowable 
-- run s2 ~~> [Just True,Just True,Just False,Just False,Just False]

s3 = liftM fa (the mallowable) <*> (return odd)
-- the allowable number is odd
-- run s3 ~~> [Nothing,Just True,Nothing,Nothing,Nothing]

s4 = mand s1 s2
-- 1 is allowable and 2 is not allowable
-- run s4 ~~> [Just False,Just True,Just False,Just False,Just False]

s5 = mand s4 s3
-- (1 is allowable and 2 is not allowable) and the allowable number is odd
-- run s5 ~~> [Just False,Just True,Just False,Just False,Just False]

s6 = mand s3 s4
-- the allowable number is odd and (1 is allowable and 2 is not allowable)
-- run s6 ~~> [Nothing,Just True,Nothing,Nothing,Nothing]
