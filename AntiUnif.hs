{-# LANGUAGE FlexibleInstances #-}
module AntiUnif where

import Types
import Sub
import Renameable
import Control.Applicative
import Control.Monad.State

class Subable a => AntiUnif a where
  antiUnif :: a -> a -> StateT ID Maybe (Unifier,Unifier,a)

instance AntiUnif Term where
  antiUnif (V i)           (Symbol n ts)   = return $ ([]                 ,[(i,Symbol n ts)],V i)
  antiUnif (Symbol n ts)   (V i)           = return $ ([(i,Symbol n ts)],[]                 ,V i)
  antiUnif (Symbol ln lts) (Symbol rn rts) = do
    i <- get
    modify (+1)
    case ln == rn of
      False -> pure ([(i,Symbol ln lts)],[(i,Symbol rn rts)],V i)
      True  -> do
        (lsubs,rsubs,ts) <- antiUnif lts rts :: StateT ID Maybe (Unifier,Unifier,[Term])
        return (lsubs,rsubs,Symbol ln ts)

instance AntiUnif a => AntiUnif[a] where
  antiUnif [] [] = return ([],[],[])
  antiUnif (x:xs) (y:ys) = do
    (lsub1,rsub1,p)  <- antiUnif x y
    (lsub2,rsub2,ps) <- antiUnif (map (applyUnif lsub1) xs) (map (applyUnif rsub1) ys)
    return (lsub1 ++ lsub2,rsub1 ++ rsub2,p:ps)


instance AntiUnif Clause where
  antiUnif = undefined
