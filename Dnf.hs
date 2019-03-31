module Dnf where

import Types

dnfify :: [Clause] -> [DjClause]
dnfify csin = cs
  where
    pss = sequence . map (uncurry (++)) $ csin
    cs = [ (filter getBool ps,filter (not . getBool) ps) | ps <- pss ]

getBool :: Predicate -> Bool
getBool (P b _ _) = b
