{-# LANGUAGE FlexibleInstances #-}
module AntiUnif where

import Types
import Sub
import Renameable
import Control.Applicative
import Control.Monad.State
import Data.Tuple.Extra
import Unif
import ShowTex

safeJoinSubs :: Sub -> Sub -> Maybe (Either Sub (Sub,Sub))
safeJoinSubs l@(li,lt) r@(ri,rt) 
  | li /= ri = Just . Right $ (l,r)
  | otherwise = do
    unif <- unify lt rt
    return $ Left (li,applyUnif unif lt)

safeAddSub :: Sub -> Unifier -> Maybe Unifier
safeAddSub s [] = Just [s]
safeAddSub s (u:us) = do
  r <- safeJoinSubs s u
  case r of
    Left s -> safeAddSub s us
    Right (l@(li,lt),r@(ri,rt)) -> do
      us' <-  (safeAddSub (li,applySub r lt) us)
      return $ (ri,applySub l rt) : us'

safeJoin :: Unifier -> Unifier -> Maybe Unifier
safeJoin [] ys = Just ys
safeJoin (x:xs) ys = do
  ys' <- (safeAddSub x ys)
  safeJoin xs ys'

safeConcat :: [Unifier] -> Maybe Unifier
safeConcat [] = Just []
safeConcat (x1:[]) = Just x1
safeConcat (x1:x2:xs) = do
  x <- safeJoin x1 x2
  safeConcat (x:xs)

class Subable a => AntiUnif a where
  antiUnif :: a -> a -> StateT ID Maybe (Unifier,Unifier,a)

instance AntiUnif Term where
  antiUnif (V l)           (V r)           = return $ ([]                ,[(l,V r)]       ,V l)
  antiUnif (V i)           (Symbol n ts)   = return $ ([]               ,[(i,Symbol n ts)],V i)
  antiUnif (Symbol n ts)   (V i)           = return $ ([(i,Symbol n ts)],[]               ,V i)
  antiUnif (Symbol ln lts) (Symbol rn rts) = do
    i <- get
    modify (+1)
    if ln == rn 
      then do
        (lsubs,rsubs,ts) <- antiUnif lts rts :: StateT ID Maybe (Unifier,Unifier,[Term])
        return (lsubs,rsubs,Symbol ln ts)
      else pure ([(i,Symbol ln lts)],[(i,Symbol rn rts)],V i)

instance AntiUnif a => AntiUnif[a] where
  antiUnif xs ys = do
    (lsubss,rsubss,ps) <- fmap unzip3 .  sequence $zipWith antiUnif xs ys
    lsubs <- lift $ safeConcat lsubss
    rsubs <- lift $ safeConcat rsubss
    return (lsubs,rsubs,ps)


instance AntiUnif Clause where
  antiUnif = undefined


-- test Cases

a = [Symbol 1 [] , V 1          , Symbol 3 [] ]
b = [Symbol 1 [] , Symbol 2 []  , V 2         ]
c = [Symbol 1 [] , Symbol 2 []  , Symbol 4 [] ]

test :: IO ()
test = do
  print $ a
  print $ b
  print $ c
  print $ runStateT (antiUnif a b) 5
  print $ runStateT (antiUnif a c) 5
  print $ runStateT (antiUnif b c) 5
  




