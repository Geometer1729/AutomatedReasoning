{-# LANGUAGE FlexibleInstances , Rank2Types #-}
module UnifSub where

import Types
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Function

type Sub = (ID,Term) -- the variable id and the term
type Unifier = Maybe [Sub] -- the list of substitutions 

{-
 - Classes
 -}

class Subable a where -- substitutions can be applied
  applySub :: Sub -> a -> a

class Subable a => Unifiable a where 
  unify :: a -> a -> Unifier

class Subable a => Subsumable a where
  subsumes' :: a -> a -> Unifier

class Subable a => Renamable a where -- Can be renamed
  -- the ID is the free Id 
  -- THe [ID] is the IDs which have already been renamed
  getSubs :: a -> State (ID,[ID]) [Sub]

class Nameable a where 
  -- get a free variable name where all larger IDs are also free
  getFree :: a -> ID

{-
 - Generic Instances
 -}

instance Subable a => Subable [a] where
  applySub subs = map $ applySub subs

instance Renamable a => Renamable [a] where
  -- no subs are nesecary for the empty list
  getSubs [] = return []
  getSubs (x:xs) = do
    -- get subs for first term
    subs <- getSubs x
    -- get subs for subsequent terms
    subs' <- getSubs xs
    -- return all subs
    return (subs ++ subs')

instance Nameable a => Nameable [a] where
  getFree = maximum . map getFree 

{-
 - Term Instances
 -}

instance Subable Term where
  -- if the variable is the one being substituted replace it else leave it
  applySub (id,t) v@(V iv) = if id == iv then t else v
  -- apply the substitution to each argument
  applySub sub (Symbol ids ts) = Symbol ids (map (applySub sub) ts)

instance Unifiable Term where
  -- for a Variable and term substitute the variable to the term
  unify (V i) t     = Just [(i,t)] 
  unify t     (V i) = Just [(i,t)]
  -- if the functions have the same ID unify the arguments
  unify (Symbol li lts) (Symbol ri rts) = (guard (li == ri)) *> unify lts rts

instance Subsumable Term where
  -- Variable subsumes any term by unifiying to if
  subsumes' (V id) t = Just [(id,t)] 
  -- If the function or constant has the same name subsumption is passed down to the arguments
  subsumes' (Symbol li lts) (Symbol ri rts) = (guard $ li == ri) *> subsumes' lts rts
  -- A sysmbol can't subsume a variable
  subsumes' (Symbol _ _) (V _) = Nothing

instance Renamable Term where
  getSubs (V i) = do
    -- get the free ID and the handled IDs
    (fi,is) <- get
    if i `elem` is then
      return [] -- return [] as no further subbs are nesecary
    else do
    -- increment the free ID and append the variable's ID to the handled IDS
    put (fi + 1,i:is)
    -- return the new substitution sending the variable to the old free ID
    return [(i,V fi)]
  -- Symbols shouldn't be renamed so just get the subs to rename the arguments
  getSubs (Symbol _ ts) = getSubs ts

instance Nameable Term where
  getFree (V i) = i + 1
  getFree (Symbol _ ts) = getFree ts

{-
 - [Term] Instances
 -}

instance Unifiable [Term] where
  -- empty list is unified by nothing
  unify [] [] = Just []
  unify (tl:tsl) (tr:tsr) = do
    subs <- unify tl tr -- subs to unify first term from each list 
    subs' <- on unify (applySubs subs) tsl tsr -- subs to unify the remainder of the list
    return (subs ++ subs') -- return all the substitutions
  unify _ _ = error "unify on a list of terms expects lists of the same length"

instance Subsumable [Term] where -- Subsume each term in the right list with the coresponding term in the left list
  -- If there are no terms subsumption happens with no unifier
  subsumes' [] [] = Just []
  subsumes' (lt:lts) (rt:rts) = do
    -- get the substitutions required to subsume the first right term with the first left term
    subs <- subsumes' lt rt
    -- apply those substitutions and then subsume the remaining list of terms
    subs' <- on subsumes' (applySubs subs) lts rts
    -- return all of the nesecary subsumptions
    return (subs ++ subs')
  subsumes' _ _ = error "tried to subsume uneven lists of terms"

{-
 - Preidicate Instances
 -}
 
instance Subable Predicate where
  applySub sub (P b i ts) = P b i (applySub sub ts) -- apply over the terms

instance Subsumable Predicate where
  -- as long as the negation is the same and the function id is the same just subsume the arguments
  subsumes' (P lb li lts) (P rb ri rts) = (guard $ (lb == rb) && (li == ri)) *> subsumes' lts rts

instance Renamable Predicate where
  -- predicates don't need renaming 
  getSubs (P _ _ ts) = getSubs ts

instance Nameable Predicate where
  getFree (P _ _ ts) = getFree ts

{-
 - [Predicate] Instance 
 -}

instance Subsumable [Predicate] where -- subsume some term in the right list with each term from the left list
  subsumes' (x:xs) rs = listToMaybe . catMaybes $ do -- list monad
    (y,ys) <- getEntsWithRest rs
    return $ do -- maybe monad
      subs <- subsumes' x y
      subs' <- on subsumes' (applySubs subs) xs ys
      return (subs ++ subs')

{-
 - Clause Instances 
 -}

instance Subable Clause where
  applySub u (ls,rs) = (applySub u ls,applySub u rs)

instance Subsumable Clause where 
  -- concatenates the clause to a list of predicates and use the [Predicate instance
  subsumes' = on subsumes' (uncurry (++))

instance Subable (Clause,a) where
  applySub s (x,y) = (applySub s x,y)

instance Subsumable (Clause,a) where
  subsumes' (l,_) (r,_) = subsumes' l r

{-
 - Utility Functions
 -}

getLeftPairs :: ([a],[a]) -> [(a,[a])]
-- gets every pair of one element in the left and all other elements left or right
getLeftPairs (ls,rs) = [ (a,xs++rs) | (a,xs) <- getEntsWithRest ls ]

getEntsWithRest :: [a] -> [(a,[a])]
-- gets every pair of one element and all other elements
getEntsWithRest [] = []
getEntsWithRest (x:xs) = (x,xs) : [ (a,x:b) | (a,b) <- getEntsWithRest xs ]

selfPairs :: [a] -> [(a,a)]
-- returns every pair of one element from the list and a subsequent element from the list
selfPairs [] = []
selfPairs (x:xs) = [ (x,x') | x' <- xs ] ++ (selfPairs xs)

resTargets :: [a] -> [a] -> [(a,a)]
-- returns every pair of one element from the left and one from the right
-- and every pair of one element from the right and a subsequent element from the right
resTargets ls rs = [ (l,r) | l <- ls , r <- rs ] ++ (selfPairs rs)

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
    -- if the resolution is posible return a new clause with the other predicates
    u <- resolvePreds l r 
    return $ applySubs u (ls ++ rs)

{-
 -Subsumption
 -}

subsumes :: (Subsumable a) => a -> a -> Bool
subsumes l r = isJust $ subsumes' l r

{-
 - Processing
 -}

data History = Given ID | Derived ID History History

data Layer = Layer {
   procesedClauses   :: [(Clause,History)]
  ,unprocesedClauses :: [(Clause,History)]
  ,clauseFreeId :: ID -- requires all subsequent ids are free
  ,varFreeId :: ID
  }


rename :: (Renamable a) => a -> State ID a
-- the State ID is the free ID 
rename x = do
  -- get the free ID
  fi <- get
  -- get the subs and the new free ID
  -- start with [] because no IDs have been renamed
  let (subs,(fi',_)) = runState (getSubs x) (fi,[])
  -- set the new free ID
  put fi'
  -- return x with the subs applied
  return $ applySubs subs x

applySubs :: (Subable a) => [Sub] -> a -> a 
-- empty list do nothing
applySubs []     x = x
applySubs (s:ss) x = applySubs ss $ applySub s x

applyUnif :: (Unifiable a) => Unifier -> a -> Maybe a
-- apply subs if Unifier is Just else return Nothing
applyUnif unifier x = do
  subs <- unifier 
  return $ applySubs subs x 
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

stepLayer :: Layer -> Layer
-- move the unprocesed clauses to procesed and add the new clauses
stepLayer (Layer pcs ucs cid tid) = Layer ocs ncs' cid' tid'
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
-- assumes the left clauses have already been subsumed with eachother
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
initialize pss = Layer [] ([ (c,Given n) | (c,n) <- (zip cs [0..])]) (length pss) (getFree pss)
  where
    cs = map clauseArrange pss

