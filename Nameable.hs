module Nameable where

import Types

class Nameable a where 
  -- get a free variable name where all larger IDs are also free
  getFree :: a -> ID

instance Nameable a => Nameable [a] where
  -- 0 ensure the list is non empty
  getFree = maximum . (0:) . map getFree 

instance Nameable Term where
  getFree (V i) = i + 1
  getFree (Symbol _ ts) = getFree ts

instance Nameable Predicate where
  getFree (P _ _ ts) = getFree ts

