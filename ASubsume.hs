{-# LANGUAGE FlexibleInstances , MultiParamTypeClasses #-}
module ASubsume where
import Types
import Sub
import Subsume
import Data.Maybe

-- Asymetric Subsumable 
class ASubsumable a b where
  asubsumes' :: a -> b -> Unifier
  asubsumes  :: a -> b -> Bool
  asubsumes l r = isJust $ asubsumes' l r


instance ASubsumable Schema Predicate where
  -- atempt to subsume the target directly then try to subsume it with each of the implications
  asubsumes' (is,p) targ = listToMaybe . catMaybes $ (subsumes' p targ) : do -- list monad
      -- get each implication
      (a,b) <- is
      return $ do -- maybe monad
        -- get a unifier which causes the outcome of the implication to subsume the target
        subs <- subsumes' b targ
        -- attempt to unify what the term would have had to have been for the 
        -- implication to turn it into the target
        asubsumes' (is,p) (applySubs subs a)

instance ASubsumable Schema [Predicate] where
  asubsumes' a [b] = asubsumes' a b
  asubsumes' _ _ = Nothing

instance ASubsumable Schema Clause where
  asubsumes' a (b,c) = asubsumes' a (b ++ c)
