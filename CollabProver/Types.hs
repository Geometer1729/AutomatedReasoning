module Types where

{-
 - PARSING LAND
 -}
data Formula = Var String
              |Function String [Formula]
              |And [Formula]
              |Or [Formula]
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
data Predicate = P Bool ID [Term]

instance Eq Predicate where
  (==) = undefined

instance Ord Predicate where
  compare = undefined

--Symbols represent functions and constants.
--Constants are nullary functions.
data Term = V ID
           |Symbol ID [Term]
            deriving (Show)

instance Eq Term where
  (==) = undefined

instance Ord Term where
  compare = undefined

--An `or` of a list of predicates
type Clause = ([Predicate],[Predicate]) --resolvable terms other terms
