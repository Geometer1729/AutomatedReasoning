{-# LANGUAGE FlexibleInstances #-}
module Unif where

import Types
import Control.Monad

type Unifier = Maybe [Sub] -- the list of substitutions 

type Sub = (ID,Term) -- the variable id and the term

class Unifiable a where
  unify :: a -> a -> Unifier
  apply :: Unifier -> a -> Maybe a 

instance Unifiable Term where
  unify (V i) t     = Just [(i,t)]
  unify t     (V i) = Just [(i,t)]
  unify (Symbol il tsl) (Symbol ir tsr) = (guard (il == ir)) *> unify tsl tsr

instance Unifiable [Term] where
  unify (tl:tsl) (tr:tsr) = 

instance Unifiable 
