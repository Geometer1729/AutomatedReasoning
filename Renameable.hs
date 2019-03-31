module Renameable where
import Sub
import Types
import Control.Monad
import Control.Monad.State

class Subable a => Renamable a where -- Can be renamed
  -- the ID is the free Id 
  -- THe [ID] is the IDs which have already been renamed
  getSubs :: a -> State (ID,[ID]) [Sub]

instance Renamable a => Renamable [a] where
  -- no subs are necessary for the empty list
  getSubs [] = return []
  getSubs (x:xs) = do
    -- get subs for first term
    subs <- getSubs x
    -- get subs for subsequent terms
    subs' <- getSubs xs
    -- return all subs
    return (subs ++ subs')

instance Renamable Term where
  getSubs (V i) = do
    -- get the free ID and the handled IDs
    (fi,is) <- get
    if i `elem` is then
      return [] -- return [] as no further subs are nesecary
    else do
    -- increment the free ID and append the variable's ID to the handled IDS
    put (fi + 1,i:is)
    -- return the new substitution sending the variable to the old free ID
    return [(i,V fi)]
  -- Symbols shouldn't be renamed so just get the subs to rename the arguments
  getSubs (Symbol _ ts) = getSubs ts

instance Renamable Predicate where
  -- predicates don't need renaming 
  getSubs (P _ _ ts) = getSubs ts

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

