module Dnf where

import Data.Maybe
import Processing
import Types

dnfify :: [[Predicate]] -> [DjClause]
dnfify csin = cs
  where
    pss = sequence csin
    cs = [ (filter getBool ps,filter (not . getBool) ps) | ps <- pss ]

getBool :: Predicate -> Bool
getBool (P b _ _) = b

solve :: DjClause -> Bool
solve (xs,ys) = not $ or [isJust $ resolvePreds x y | x <- xs, y <- ys]

reduce :: [DjClause] -> [DjClause]
reduce = filter solve
