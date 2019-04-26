{-# LANGUAGE FlexibleInstances #-}
module AntiUnif where

import Types
import Sub
import Renameable
import Control.Applicative
import Control.Monad.State

class AntiUnif a where
  antiUnif :: a -> a -> StateT ID Maybe (Unifier,Unifier,a)

nextFree :: State ID ID
nextFree = do
  id <- get
  modify (+1)
  return id

instance AntiUnif Term where
  antiUnif (V i)           (Symbol n ts)   = return $ Just ([]                 ,[(i,Symbol n ts)],V i)
  antiUnif (Symbol n ts)   (V i)           = return $ Just ([(i,Symbol n ts)],[]                 ,V i)
  antiUnif (Symbol ln lts) (Symbol rn rts) = do
    i <- nextFree
    return $ case ln == rn of
      False -> pure ([(i,Symbol ln lts)],[(i,Symbol rn rts)],V i)
      True  -> do
        (lsubs,rsubs,ts) <- antiUnif ltr rts
        return (lsubs,rsubs,Sybol ln ts)
        


instance AntiUnif Clause where
  antiUnif = undefined
