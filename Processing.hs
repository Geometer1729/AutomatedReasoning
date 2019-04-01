{-# LANGUAGE FlexibleInstances #-}
module Processing where

import Data.Bits
import ASubsume 
import Types
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

{-
 - Resolution
 -}

resolvePreds :: Predicate -> Predicate -> Unifier
-- return the unifier required to resolve to predicates
resolvePreds (P lb li lts) (P rb ri rts) = (guard $ (lb /= rb) && (li == ri)) *> unify lts rts

resolve :: Clause -> Clause -> [[Predicate]]
resolve lc rc = catMaybes $ do -- list monad
  -- all pairs of a resolvable predicate and all other predicates from both clauses
  (l,ls) <- getLeftPairs lc 
  (r,rs) <- getLeftPairs rc 
  return $ do -- maybe monad
    -- if the resolution is possible return a new clause with the other predicates
    u <- resolvePreds l r 
    return $ applySubs u (ls ++ rs)

resolveHist :: (Clause,History) -> (Clause,History) -> State (ID,ID) [(Clause,History)]
-- returns resolved clauses with their histories
-- state is the Free id for the clauses and then terms
resolveHist (lc,lh) (rc,rh) = do
  (cid,tid) <- get
  -- the lists of predicates which make up the new clauses
  let predicateLists = [ (ps,Derived id lh rh) | (ps,id) <- zip (resolve lc rc) [cid..] ] :: [([Predicate],History)]
  -- the predicates but renamed
  let (renamed,tid') = runState ( sequence [ fmap (\x -> (x,h)) (rename p)   | (p,h) <- predicateLists ] ) tid :: ([([Predicate],History)],ID)
  -- new clauses
  let ncs = [ (clauseArrange p , h) | (p,h) <- renamed ]
  put (cid + (length predicateLists),tid')
  return ncs

schemaSubsumption :: [Schema] -> [(Clause,History)] -> [(Clause,History)]
schemaSubsumption ss cs = [ (c,h) | (c,h) <- cs , not . or $ [ asubsumes s c| s <- ss ] ]

updateSchema :: Layer -> Layer
updateSchema (Layer pcs ucs ss cid tid) = Layer  pcs' ucs' ss' cid tid
  where
    ss' = learnSchema (pcs ++ ucs ++ bases)
    bases = map (\x -> ([x],[])) . map snd $ ss -- get bases of current schema
    pcs' = schemaSubsumption ss' pcs
    ucs' = schemaSubsumption ss' ucs 

stepLayer :: Layer -> Layer
-- move the unprocesed clauses to processed and add the new clauses
stepLayer (Layer pcs ucs ss cid tid) = Layer ocs ncs' ss cid' tid'
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
-- No schema are assumed
-- The clause namespace is used up to the length of the given list
initialize pss = Layer [] ([ (c,Given n) | (c,n) <- (zip cs [0..])]) [] (length pss) (getFree pss)
  where
    cs = map clauseArrange pss

{-
 - Schemea
 -}

learnSchema :: [Clause] -> [Schema]
learnSchema cs = applicable (getImplications cs) (getSingletons cs)

getImplications :: [Clause] -> [Implication]
getImplications cs = concat $ do
  (l,r) <- cs
  return $ case l ++ r of
      [a,b] -> (scheme a b ++ scheme b a)
      _ -> []

scheme :: Predicate -> Predicate -> [(Predicate,Predicate)]
scheme l@(P lb ln lts) r@(P rb rn rts)
  -- If the names are the same
  -- and the booleans disagree
  -- and the left terms subsume the right terms
  | (ln == rn) && (lb /= rb) && (subsumes lts rts) = [(l , P lb rn rts)]
  | otherwise = []
    
applicable :: [Implication] -> [Predicate] -> [Schema]
applicable is ps = [ ([i | i@(l,r) <- is , subsumes l p],p) | p <- ps ]
  where 

getSingletons :: [Clause] -> [Predicate]
getSingletons cs = [ head l | (l,r) <- cs , length l == 1 , null r]

