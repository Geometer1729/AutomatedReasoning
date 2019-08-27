{-# LANGUAGE FlexibleInstances #-}
module Types where

import BinTree
import Data.List
import qualified Data.Map as M

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
type Clause = [Predicate] --resolvable terms other terms

clauseArrange :: Clause -> (Clause,Clause)
clauseArrange xs = case group . sort $ xs of
  (y:ys) -> (y,concat ys)
  [] -> ([],[])

type History = BinTree ID

type Implication = ([Clause],Clause)

type Candidate = BinTree Clause

type Table = M.Map ID Clause

data Layer = Layer {
   processedClauses   :: [(Clause,History)]
  ,processedImplications :: [Implication]
  ,unprocessedClauses :: [(Clause,History)]
  ,unprocessedImplications :: [Implication]
  ,table :: M.Map ID Clause
  ,nextFreeClauseID :: ID -- requires all subsequent ids are free
  ,nextFreeVarID :: ID
  } 


type Sub = (ID,Term) -- the variable id and the term
type Unifier = [Sub] -- the list of substitutions 
type Schema = ([(Predicate,Predicate)],[Predicate]) 
-- The left list is a list of patterns which imply other patterns 
-- The right list is the list of arguments over which the left list can be iterated

-- show instances here to avoid Orphan instances

instance Show Predicate where
  show (P True i []) = "P" ++ show i
  show (P True i lt) = "P" ++ show i ++ "(" ++ show lt ++ ")"
  show (P False i []) = "~P" ++ show i
  show (P False i lt) = "~P" ++ show i ++ "(" ++ show lt ++ ")"

instance {-# OVERLAPPING #-} Show [Predicate] where
  show [p] = show p
  show (p:l) = show p ++ ", " ++ show l
  show [] = ""

instance Show Term where
  show (V i) = "X" ++ show i
  show (Symbol i []) = "a" ++ show i
  show (Symbol i tl) = "f" ++ show i ++ "(" ++ show tl ++ ")"

instance {-# OVERLAPPING #-} Show [Term] where
  show [t] = show t
  show (t:l) = show t ++ ", " ++ show l
  show [] = error "Attempt to show an empty list of terms"


instance Show Layer where
  show l = unlines $ "begin layer\nProcesed" : (map chShow ps) ++ ["Unprocessed"] ++ (map chShow us) ++ ["end layer"]
    where
      ps = processedClauses l 
      us = unprocessedClauses l
      chShow :: (Clause,History) -> String
      chShow (c,_) = unlines [show c]
