{-# LANGUAGE FlexibleInstances #-}
module Processing where

import Types
import BinTree
import ShowTex
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Function
import Sub
import Subsume
import Unif
import Renameable
import Nameable
import ListUtil
import qualified Data.Map as M


{-
 - Resolution
 -}

resolvePreds :: Predicate -> Predicate -> Maybe Unifier
-- return the unifier required to resolve to predicates
resolvePreds (P lb li lts) (P rb ri rts) = (guard $ (lb /= rb) && (li == ri)) *> unify lts rts

resolve :: Clause -> Clause -> [[Predicate]]
resolve lc rc = catMaybes $ do -- list monad
  -- all pairs of a resolvable predicate and all other predicates from both clauses
  (l,ls) <- getLeftPairs . clauseArrange $lc 
  (r,rs) <- getLeftPairs . clauseArrange $rc 
  return $ do -- maybe monad
    -- if the resolution is possible return a new clause with the other predicates
    u <- resolvePreds l r 
    return $ applyUnif u (ls ++ rs)

resolveHist :: (Clause,History) -> (Clause,History) -> State (ID,ID) [(Clause,History)]
-- returns resolved clauses with their histories
-- state is the Free id for the clauses and then terms
resolveHist (lc,lh) (rc,rh) = do
  (cid,tid) <- get
  -- the lists of predicates which make up the new clauses
  let predicateLists = [ (ps,Node id lh rh) | (ps,id) <- zip (resolve lc rc) [cid..] ] :: [([Predicate],History)]
  -- the predicates but renamed
  let (renamed,tid') = runState ( sequence [ fmap (\x -> (x,h)) (rename p)   | (p,h) <- predicateLists ] ) tid :: ([([Predicate],History)],ID)
  -- new clauses
  put (cid + (length predicateLists),tid')
  return renamed

stepLayer :: Layer -> Layer
-- move the unprocesed clauses to processed and add the new clauses
stepLayer (Layer pcs _ ucs _ _ cid tid) = Layer ocs [] ncs' [] M.empty cid' tid'
  where
    -- list of all pairs which need to be resolved
    targs = resTargets pcs ucs
    -- the list of new clauses and their histories from each pair of clauses
    -- and the new free ids
    (ncss,(cid',tid')) = runState (sequence [ resolveHist l r | (l,r) <- targs ]) (cid,tid)
    -- concatenated down to just a list of clauses
    ncs = concat ncss
    (ocs,ncs') = subsumption (pcs ++ ucs) ncs

anySubsume :: (Subsumable a) => [a] -> a -> Bool
-- checks if the second argument is subsumed by any of the things in the list
anySubsume xs y = or [ subsumes x y | x <- xs ]

removeSubsumedBy :: (Subsumable a) => [a] -> [a] -> [a]
-- remove everything in the second list which is subsumed by anything in the second list
removeSubsumedBy xs ys = [ x | x <- xs , not $ anySubsume ys x ]

subsumption :: (Subsumable a) => [a] -> [a] -> ([a],[a])
-- assumes the left clauses have already been subsumed with each other
subsumption ls rs = (ls',rs'')
  where
    -- remove lefts subsumed by anything in rs
    ls'  = removeSubsumedBy ls rs
    -- remove anything in rs subsumed by something in ls
    rs'  = removeSubsumedBy rs ls'
    -- remove anything in rs subsumed by a latter clause in rs
    rs'' = [ r | (r,rrs) <- getEntsWithRest rs' , not $ anySubsume rrs r ]

initialize :: [[Predicate]] -> Layer
-- no clauses have been processed all are new and all are given
-- The clause namespace is used up to the length of the given list
initialize pss = Layer [] [] ([ (c,Leaf n) | (c,n) <- (zip pss [0..])]) [] M.empty (length pss) (getFree pss)

{-
 - Schemea
 -}

--getApplicables :: Predicate -> [Clause] -> [Predicate]
--getApplicables p cs = filter (unifies p) singletons
--  where 
--    singletons = [ head l | (l,r) <- cs , length l == 1 , null r , unifies p (head l)]
    

