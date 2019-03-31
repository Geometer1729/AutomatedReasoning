{-# LANGUAGE FlexibleInstances #-}
module Unif where

import Types
import Sub
import Data.Maybe
import Control.Monad
import Data.Function

class Subable a => Unifiable a where 
  unify :: a -> a -> Unifier
  unifies :: a -> a -> Bool
  unifies x y = isJust $ unify x y

instance Unifiable Term where
  -- for a Variable and term, substitute the variable to the term
  unify (V i) t     = Just [(i,t)] 
  unify t     (V i) = Just [(i,t)]
  -- if the functions have the same ID unify the arguments
  unify (Symbol li lts) (Symbol ri rts) = (guard (li == ri)) *> unify lts rts

instance Unifiable [Term] where
  -- empty list is unified by nothing
  unify [] [] = Just []
  unify (tl:tsl) (tr:tsr) = do
    subs <- unify tl tr -- subs to unify first term from each list 
    subs' <- on unify (applySubs subs) tsl tsr -- subs to unify the remainder of the list
    return (subs ++ subs') -- return all the substitutions
  unify _ _ = error "unify on a list of terms expects lists of the same length"

instance Unifiable Predicate where
  unify (P lb ln lts) (P rb rn rts) = guard (lb == rb && ln == rn) *> unify lts rts

applyUnif :: (Unifiable a) => Unifier -> a -> Maybe a
-- apply subs if Unifier is Just else return Nothing
applyUnif unifier x = do
  subs <- unifier 
  return $ applySubs subs x 
