module Completion where

import Equation
{-
type Equation = (Term,Term)
data Term =  Var String
           | Function String [Term]
            deriving (Eq,Show)
-}

type Unifier = [(String,Term)]

uTest0 :: Maybe Unifier
uTest0 = let t1 = readTerm "f(x,y,g(a()))"
             t2 = readTerm "f(g(y),h(x),g(a()))"
         in unify t1 t2

unify :: Term -> Term -> Maybe Unifier
unify (Function s1 args1) (Function s2 args2)
    | s1 /= s2 = Nothing
    | s1 == s2 = let us = map (uncurry unify) $ zip args1 args2
                 in foldl combineUnifiers (Just []) us
unify (Var s1) t1 
    | occursCheck s1 t1 = Nothing
    | otherwise = Just [(s1,t1)]            
unify t1 (Var s1) = unify (Var s1) t1

--Checks if the variable s occurs in the term t
occursCheck :: String -> Term -> Bool
occursCheck s (Var s') = s == s'
occursCheck s (Function _ args) = or [occursCheck s t | t <- args]

combineUnifiers :: Maybe Unifier -> Maybe Unifier -> Maybe Unifier
combineUnifiers Nothing _ = Nothing
combineUnifiers _ Nothing = Nothing
combineUnifiers (Just u1) (Just []) = Just u1
combineUnifiers (Just u1) (Just ((s',t'):u2s)) = if or [s == s' && t /= t' | (s,t) <- u1] then Nothing else combineUnifiers (Just (if (s',t') `elem` u1 then u1 else (s',t'):u1)) (Just u2s)

criticalPair :: Term -> Term -> Maybe (Term,Term)
criticalPair t1 t2 = undefined
