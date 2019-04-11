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

type Implication = ([Clause],Clause) -- A list of hypotheses and a the conclusion
type Scheme = ([Implication],[Clause]) -- the list of applicable implications and the base clauses

instance ASubsumable Clause Implication where
  asubsumes' c (hs,con) = subsumes c con
  -- is this actually sufficent?


  
