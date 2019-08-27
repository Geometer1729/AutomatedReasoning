{-# LANGUAGE FlexibleInstances #-}
module Processing where

import Types
import BinTree
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Sub
import Subsume
import Unif
import Renameable
import Nameable
import ListUtil
import Control.Comonad
import qualified Data.Map as M
import Util
import Tautology


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
  let predicateLists = [ (ps,Node i lh rh) | (ps,i) <- zip (resolve lc rc) [cid..] ] :: [([Predicate],History)]
  -- the predicates but renamed
  let (renamed,tid') = runState ( sequence [ fmap (\x -> (x,h)) (rename p)   | (p,h) <- predicateLists ] ) tid :: ([([Predicate],History)],ID)
  -- new clauses
  put (cid + (length predicateLists),tid')
  return renamed

stepLayer :: State Layer ()
stepLayer = do
  resolution
  subsumption
  tautologyRemoval


tautologyRemoval :: State Layer ()
tautologyRemoval = do
  us <- fmap unprocessedClauses get
  modify $ \l -> l{unprocessedClauses = filter (not . isTautology . fst) us } 

resolution :: State Layer ()
resolution = do
  ups <- fmap unprocessedClauses get
  ps  <- fmap processedClauses   get
  let targs = resTargets ps ups
  ncs <- fmap concat $ stateIDLift  $ sequence [resolveHist l r | (l,r) <- targs ]
  stateTableLift $ updateCmap ncs
  modify $ \l -> l{processedClauses=ps ++ ups,unprocessedClauses = ncs}


  



--  stepLayer :: Layer -> Layer
--  -- move the unprocesed clauses to processed and add the new clauses
--  stepLayer (Layer pcs pimps ucs uimps cmap cid tid) = Layer ocs newpimps ncs' newuimps cmap' cid' tid'
--    where
--      -- list of all pairs which need to be resolved
--      targs = resTargets pcs ucs
--      -- the list of new clauses and their histories from each pair of clauses
--      -- and the new free ids
--      (ncss,(cid',tid')) = runState (sequence [ resolveHist l r | (l,r) <- targs ]) (cid,tid)
--      -- concatenated down to just a list of clauses
--      ncs = concat ncss
--      (ocs,ncs') = subsumption (pcs ++ ucs) ncs
--      cmap' = updateCmap cmap ncs
--      newpimps = pimps ++ uimps
--      newuimps = genImps cmap' (map snd (pcs ++ ucs))

updateCmap :: [(Clause,History)] -> State (M.Map ID Clause) ()
updateCmap = mapM_ addClauseCmap
  where
    addClauseCmap :: (Clause,History) -> State (M.Map ID Clause) ()
    addClauseCmap (c,h) = modify $ M.insert (extract h) c 

anySubsume :: (Subsumable a) => [a] -> a -> Bool
-- checks if the second argument is subsumed by any of the things in the list
anySubsume xs y = or [ subsumes x y | x <- xs ]

removeSubsumedBy :: (Subsumable a) => [a] -> [a] -> [a]
-- remove everything in the first list which is subsumed by anything in the second list
removeSubsumedBy xs ys = [ x | x <- xs , not $ anySubsume ys x ]

subsumption :: State Layer ()
subsumption = do
  l <- get
  let ups = unprocessedClauses l
  let ps = processedClauses l
  let (ps',ups') = generalSubsumption ps ups
  put l{processedClauses = ps' , unprocessedClauses = ups' }

generalSubsumption :: (Subsumable a) => [a] -> [a] -> ([a],[a])
-- assumes the left clauses have already been subsumed with each other
generalSubsumption ls rs = (ls',rs'')
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
initialize pss = Layer [] [] ([ (c,Leaf n) | (c,n) <- (zip pss [0..])]) [] (M.fromList $ zip [0..] pss) (length pss) (getFree pss)

{-
 - Schemea
 -}

--getApplicables :: Predicate -> [Clause] -> [Predicate]
--getApplicables p cs = filter (unifies p) singletons
--  where 
--    singletons = [ head l | (l,r) <- cs , length l == 1 , null r , unifies p (head l)]
    

