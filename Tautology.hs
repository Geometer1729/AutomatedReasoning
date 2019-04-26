
module Tautology (isTautology) where

import Control.Comonad
import Control.Monad
import Data.Function
import Data.List.NonEmpty (NonEmpty(..))

import Types

isTautology :: Clause -> Bool
isTautology [] = False
isTautology (a:as) = or $ (a:|as) =>> \(t:|ts) -> not $ null $ filter ((==t) . negPred) ts

negPred :: Predicate -> Predicate
negPred (P b n ls) = P (not b) n ls

