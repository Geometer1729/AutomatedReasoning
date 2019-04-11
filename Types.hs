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
data Predicate = P Bool ID [Term] deriving(Eq,Ord)
--deriving Eq and Ord do lexigraphic comparison by default, prefering negative clauses

--Symbols represent functions and constants.
--Constants are nullary functions.
data Term = V ID
           |Symbol ID [Term]

instance Eq Term where
  (V _) == _ = True
  _ == (V _) = True
  (Symbol li lts) == (Symbol ri rts) = li == ri && lts == rts

instance Ord Term where
  compare (V _) _ = EQ
  compare _ (V _) = EQ
  compare (Symbol li lts) (Symbol ri rts) = case compare li ri of
    LT -> LT
    GT -> GT 
    EQ -> compare lts rts

--An `or` of a list of predicates
type Clause = [Predicate]
type SplitClause = ([Predicate],[Predicate]) --resolvable terms other terms

splitClause :: [Predicate] -> Clause
splitClause xs = case group . sort $ xs of
  (y:ys) -> (y,concat ys)
  [] -> ([],[])

data History = Given ID | Derived ID History History 

data Layer = Layer {
   processedClauses   :: [(Clause,History)]
  ,unprocessedClauses :: [(Clause,History)]
  ,implicatiosn :: [Implication]
  ,nextFreeClauseID :: ID -- requires all subsequent ids are free
  ,nextFreeVarID :: ID
  } 

type Sub = (ID,Term) -- the variable id and the term
type Unifier = [Sub] -- the list of substitutions 

type Implication = ([Clause],Clause) -- A list of hypotheses and a the conclusion
type Scheme = ([Implication],[Clause]) -- the list of applicable implications and the base clauses

-- The left list is a list of patterns which implie other paterns 
-- The right list is the list of arguments over which the left list can be itterated
