
module Tautology (isTautology) where

import Control.Comonad
import Control.Monad
import Data.Function
import Data.List.NonEmpty (NonEmpty(..))

import Types

isTautology :: Clause -> Bool
isTautology (xs,ys) = isT $ xs ++ ys
  where 
    isT [] = False
    isT (a:as) = or $ (a:|as) =>> \(t:|ts) -> not $ null $ filter ((==t) . negPred) ts

