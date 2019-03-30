module Types where
import Data.List

{-
 - PARSING LAND
 -}
data Formula = Var String
              |Function String [Formula]
              |And Formula Formula
              |Or Formula Formula
              |Not Formula
              |Implies Formula Formula
              |Quantified Quantifier Formula
              deriving (Eq,Show)

data Quantifier = Forall String | Exists String deriving (Eq,Show)

{- DMZ -}
type ID = Int

{-
- PROVER LAND
-}
--Represents a logical claim about an object in the Herbrand universe
--The Bool indicates whether or not the predicate is negated, the ID is the ID value, and the [Term] is the args
data Predicate = P Bool ID [Term] deriving(Eq,Ord,Show)
--deriving Eq and Ord do lexigraphic comparison by default, prefering negative clauses

-- The first ID is the variable ID of the argument 
-- Term is the output in terms of the argument
-- The second ID is the ID of the exponent
type Lambda = (ID,Term,ID)

--Symbols represent functions and constants.
--Constants are nullary functions.
--Schemes are fun
data Term = V ID
           |Symbol ID [Term]
           |Scheme [Lambda] Term
            deriving (Show)


instance Eq Term where
  (V _) == _ = True
  _ == (V _) = True
  (Symbol li lts) == (Symbol ri rts) = li == ri && lts == rts
  -- I can't think of a good way to compare Schemes
  _ == _ = True

instance Ord Term where
  compare (V _) _ = EQ
  compare _ (V _) = EQ
  compare (Symbol li lts) (Symbol ri rts) = case compare li ri of
    LT -> LT
    GT -> GT 
    EQ -> compare lts rts
  -- You would also need scheme order stuff here
  compare _ _ = EQ

--An `or` of a list of predicates
type Clause = ([Predicate],[Predicate]) --resolvable terms other terms

clauseArrange :: [Predicate] -> Clause
clauseArrange xs = case group . sort $ xs of
  (y:ys) -> (y,concat ys)
