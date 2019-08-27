{-# LANGUAGE FlexibleInstances #-}
module Subsume where

import Types
import Sub
import Data.Function
import ListUtil
import Data.Maybe
import Control.Monad

class Subable a => Subsumable a where
  subsumes' :: a -> a -> Maybe Unifier
  subsumes :: a -> a -> Bool
  subsumes l r = isJust $ subsumes' l r

instance Subsumable Term where
  -- Variable subsumes any term by unifiying to if
  subsumes' (V i) t = Just [(i,t)] 
  -- If the function or constant has the same name subsumption is passed down to the arguments
  subsumes' (Symbol li lts) (Symbol ri rts) = (guard $ li == ri) *> subsumes' lts rts
  -- A sysmbol can't subsume a variable
  subsumes' (Symbol _ _) (V _) = Nothing

instance Subsumable [Term] where -- Subsume each term in the right list with the corresponding term in the left list
  -- If there are no terms subsumption happens with no unifier
  subsumes' [] [] = Just []
  subsumes' (lt:lts) (rt:rts) = do
    -- get the substitutions required to subsume the first right term with the first left term
    subs <- subsumes' lt rt
    -- apply those substitutions and then subsume the remaining list of terms
    subs' <- on subsumes' (applyUnif subs) lts rts
    -- return all of the necessary subsumptions
    return (subs ++ subs')
  subsumes' _ _ = error "tried to subsume uneven lists of terms"

instance Subsumable Predicate where
  -- as long as the negation is the same and the function id is the same, just subsume the arguments
  subsumes' (P lb li lts) (P rb ri rts) = (guard $ (lb == rb) && (li == ri)) *> subsumes' lts rts

instance Subsumable [Predicate] where -- subsume some term in the right list with each term from the left list
  subsumes' [] _ = Just []
  subsumes' (x:xs) rs = listToMaybe . catMaybes $ do -- list monad
    (y,ys) <- getEntsWithRest rs
    return $ do -- maybe monad
      subs <- subsumes' x y
      subs' <- on subsumes' (applyUnif subs) xs ys
      return (subs ++ subs')

instance Subsumable (Clause,a) where
  subsumes' (l,_) (r,_) = subsumes' l r

