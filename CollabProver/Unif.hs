{-# LANGUAGE FlexibleInstances #-}
module Unif where

import Types
import Control.Monad
import Data.Maybe
import Data.Function

type Unifier = Maybe [Sub] -- the list of substitutions 

type Sub = (ID,Term) -- the variable id and the term

class Unifiable a where
  unify :: a -> a -> Unifier
  applySub :: Sub -> a -> a 

applySubs :: (Unifiable a) => [Sub] -> a -> a 
applySubs []     x = x
-- empty list do nothing
applySubs (s:ss) x = applySubs ss $ applySub s x

-- apply subs if Unifier is Just else return Nothing
applyUnif :: (Unifiable a) => Unifier -> a -> Maybe a
applyUnif unifier x = do
  subs <- unifier 
  return $ applySubs subs x 

instance Unifiable Term where
  -- for a Variable and term substitute the variable to the term
  unify (V i) t     = Just [(i,t)] 
  unify t     (V i) = Just [(i,t)]
  -- if the functions have the same ID unify the arguments
  unify (Symbol li lts) (Symbol ri rts) = (guard (li == ri)) *> unify lts rts
  -- if the variable is the one being substituted replace it else leave it
  applySub (id,t) v@(V iv) = if id == iv then t else v
  -- apply the substitution to each argument
  applySub sub (Symbol ids ts) = Symbol ids (map (applySub sub) ts)

instance Unifiable [Term] where
  -- empty list is unified by nothing
  unify [] [] = Just []
  unify (tl:tsl) (tr:tsr) = do
    subs <- unify tl tr -- subs to unify first term from each list 
    subs' <- on unify (applySubs subs) tsl tsr -- subs to unify the remainder of the list
    return (subs ++ subs') -- return all the substitutions
  unify _ _ = error "unify on a list of terms expects lists of the same length"
  applySub u ts = map (applySub u) ts -- apply sub by applying it to all terms

instance Unifiable Predicate where 
  unify (P lb li lts) (P rb ri rts) = (guard $ (lb == rb) && (li == ri)) *> unify lts rts -- implemented for completenes probably not usefull
  applySub sub (P b i ts) = P b i (applySub sub ts) -- apply over the terms

resolvePreds :: Predicate -> Predicate -> Unifier
-- return the unifier required to resolve to predicates
resolvePreds (P lb li lts) (P rb ri rts) = (guard $ (lb /= rb) && (li == ri)) *> unify lts rts

getLeftPairs :: ([a],[a]) -> [(a,[a])]
-- gets every pair of one element in the left and all other elements left or right
getLeftPairs (ls,rs) = [ (a,xs++rs) | (a,xs) <- getEntsWithRest ls ]

getEntsWithRest :: [a] -> [(a,[a])]
-- gets every pair of one element and all other elements
getEntsWithRest [] = []
getEntsWithRest (x:xs) = (x,xs) : [ (a,x:b) | (a,b) <- getEntsWithRest xs ]

resolve :: Clause -> Clause -> [Clause]
resolve lc rc = catMaybes $ do -- list monad
  -- all pairs of a resolvable predicate and all other predicates from both clauses
  (l,ls) <- getLeftPairs lc 
  (r,rs) <- getLeftPairs rc 
  return $ do -- maybe monad
    -- if the unification is posible return a new clause with the other predicates
    u <- unify l r 
    return $ clauseArrange $ map (applySubs u) (ls ++ rs)

resolutions :: [Clause] -> [Clause] -> [Clause]
resolutions xs ys = concat [ resolve x y | x <- xs , y <- ys ]
