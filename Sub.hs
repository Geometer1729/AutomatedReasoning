{-# LANGUAGE FlexibleInstances #-}
module Sub where

import Types

class Subable a where -- substitutions can be applied
  applySub :: Sub -> a -> a

instance Subable a => Subable [a] where
  applySub subs = map $ applySub subs

instance Subable Term where
  -- if the variable is the one being substituted, replace it; else leave it
  applySub (id,t) v@(V iv) = if id == iv then t else v
  -- apply the substitution to each argument
  applySub sub (Symbol ids ts) = Symbol ids (map (applySub sub) ts)

instance Subable Predicate where
  applySub sub (P b i ts) = P b i (applySub sub ts) -- apply over the terms

instance Subable Clause where
  applySub u (ls,rs) = (applySub u ls,applySub u rs)

instance Subable (Clause,a) where
  applySub s (x,y) = (applySub s x,y)

applySubs :: (Subable a) => [Sub] -> a -> a 
-- empty list do nothing
applySubs []     x = x
applySubs (s:ss) x = applySubs ss $ applySub s x

