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

applyUnif :: (Unifiable a) => Unifier -> a -> a 
applyUnif Nothing       x = x
applyUnif (Just [])     x = x
applyUnif (Just (s:ss)) x = applyUnif (Just ss) $ applySub s x


instance Unifiable Term where
  unify (V i) t     = Just [(i,t)]
  unify t     (V i) = Just [(i,t)]
  unify (Symbol li lts) (Symbol ri rts) = (guard (li == ri)) *> unify lts rts
  applySub (id,t) v@(V iv) = if id == iv then t else v
  applySub sub (Symbol ids ts) = Symbol ids (map (applySub sub) ts)

instance Unifiable [Term] where
  unify (tl:tsl) (tr:tsr) = do
    u <- unify tl tr
    on unify (applyUnif (Just u)) tsl tsr
  applySub u ts = map (applySub u) ts

instance Unifiable Predicate where 
  unify (P lb li lts) (P rb ri rts) = (guard $ (lb == rb) && (li == ri)) *> unify lts rts
  applySub sub (P b i ts) = P b i (map (applySub sub) ts)


resolvePreds :: Predicate -> Predicate -> Unifier
resolvePreds (P lb li lts) (P rb ri rts) = (guard $ (lb /= rb) && (li == ri)) *> unify lts rts

getLeftPairs :: ([a],[a]) -> [(a,[a])]
getLeftPairs (ls,rs) = [ (a,xs++rs) | (a,xs) <- getEntsWithRest ls ]

getEntsWithRest :: [a] -> [(a,[a])]
getEntsWithRest (x:xs) = (x,xs) : [ (a,x:b) | (a,b) <- getEntsWithRest xs ]

resolve :: Clause -> Clause -> [Clause]
resolve lc rc = catMaybes $ do
  (l,ls) <- getLeftPairs lc 
  (r,rs) <- getLeftPairs rc 
  return $ do
    u <- unify l r :: Unifier
    return $ clauseArrange $ map (applyUnif $ Just u) (ls ++ rs)


