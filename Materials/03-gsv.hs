type Predicate = String
data Ent = Alice | Bob | Carl deriving (Eq, Show)
data World = W1 | W2 | Hungry | Full deriving (Eq, Show)
type Variable = String
data Term = Constant Ent | Var Variable deriving (Eq, Show)
data Clause =   Pred Predicate Term 
              | Eq Term Term
              | Conj Clause Clause
              | Neg Clause
              | Poss Clause
              | Ex String Clause
              deriving (Eq, Show)
type Assignment = [(Variable, Ent)]
type Poss = (World, Assignment)
type Infostate = [Poss]

referent :: Poss -> Term -> Ent
referent (w,g) (Constant c) = c
referent (w,(v',a):g) (Var v) = if v == v' then a else referent (w,g) (Var v)

extension :: World -> Predicate -> Ent -> Bool
extension _ "woman" Alice = True
extension _ "man" Bob = True
extension _ "man" Carl = True
extension Hungry "hungry" Alice = True
extension _ "enter" Bob = True
extension _ "enter" Carl = True
extension _ "sit" Alice = True
extension _ "sit" Bob = True
extension W1 "closet" Alice = True
extension W1 "guilty" Bob = True
extension W2 "closet" Carl = True
extension W2 "guilty" Carl = True
extension _ _ _ = False

update :: Infostate -> Clause -> Infostate
update s (Pred p t) = [i | i@(w,g) <- s, extension w p (referent i t)]
update s (Eq t1 t2) = [i | i <- s, referent i t1 == referent i t2]
update s (Conj c1 c2) = update (update s c1) c2
update s (Neg c) = [i | i <- s, length (update [i] c) == 0]
update s (Poss c) = [i | i <- s, length (update s c) > 0]
update s (Ex v c) = 
  concat [update [(w, (v,a):g) | (w,g) <- s] c | a <- domain]

domain = [Alice, Bob, Carl]

test1 = update [(W1, [])] (Ex "x" (Pred "man" (Var "x")))
test2 = update [(W1, [])] (Ex "x" (Pred "woman" (Var "x")))
test3 = update [(W1, [])] (Ex "x" (Ex "y" (Conj (Pred "man" (Var "x"))
                                                (Pred "man" (Var "y")))))
test4 = update [(W1, [])] (Ex "x" (Ex "y" (Eq (Var "x") (Var "y"))))

-- Alice isn't hungry
test5 = update [(Hungry,[]),(Full,[])] (Neg (Pred "hungry" (Constant Alice)))

-- Alice isn't hungry.  Alice might be hungry
test6 = update [(Hungry,[]),(Full,[])] 
               (Conj (Neg (Pred "hungry" (Constant Alice)))
                     (Poss (Pred "hungry" (Constant Alice))))

-- Alice might be hungry.  Alice isn't hungry.
test7 = update [(Hungry,[]),(Full,[])] 
               (Conj (Poss (Pred "hungry" (Constant Alice)))
                     (Neg (Pred "hungry" (Constant Alice))))
 
-- Someone^x entered.  He_x sat.
test8 = update [(W1,[("x",Bob)])] 
               (Conj (Ex "x" (Pred "enter" (Var "x")))
                     (Pred "sit" (Var "x")))

-- He_x sat.  Someone^x entered.
test9 = update [(W1,[("x",Bob)])] 
               (Conj (Pred "sit" (Var "x"))
                     (Ex "x" (Pred "enter" (Var "x"))))

-- Someone^x is in the closet.  He_x might be guilty.
test10 = update [(W1,[]),(W2,[])]
                (Conj (Ex "x" (Pred "closet" (Var "x")))
                      (Poss (Pred "guilty" (Var "x"))))

-- Someone^x is in the closet who_x might be guilty.
test11 = update [(W1,[]),(W2,[])]
                (Ex "x" (Conj (Pred "closet" (Var "x"))
                              (Poss (Pred "guilty" (Var "x")))))
