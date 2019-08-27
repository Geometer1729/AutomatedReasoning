{-# LANGUAGE FlexibleInstances #-}
module Sub where

import Types

class Subable a where -- substitutions can be applied
  applySub :: Sub -> a -> a

instance Subable a => Subable [a] where
  applySub subs = map $ applySub subs

instance Subable Term where
  -- if the variable is the one being substituted, replace it; else leave it
  applySub (i,t) v@(V iv) = if i == iv then t else v
  -- apply the substitution to each argument
  applySub sub (Symbol ids ts) = Symbol ids (map (applySub sub) ts)

instance Subable Predicate where
  applySub sub (P b i ts) = P b i (applySub sub ts) -- apply over the terms

instance Subable (Clause,a) where
  applySub s (x,y) = (applySub s x,y)

applyUnif :: (Subable a) => Unifier -> a -> a 
applyUnif []     x = x
applyUnif (s:ss) x = applyUnif ss $ applySub s x

