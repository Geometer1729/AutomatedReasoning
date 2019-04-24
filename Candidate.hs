module Candidate where

import Types
import Subsume
import qualified Data.Map as M
import Data.Maybe
import BinTree
import AntiUnif
import Control.Monad
import Control.Applicative
import Control.Comonad

getCandidate :: (M.Map ID Clause) -> (Clause,BinTree ID) -> [Candidate] 
getCandidate clauses (c,h) = catMaybes [ matchCands st tree | st <- subTrees ]
  where
    subTrees = toList . duplicate $ tree
    tree = fmap getClause h :: Candidate
    getClause :: ID -> Clause
    getClause i = fromJust $ M.lookup i clauses

matchCands :: Candidate -> Candidate -> Maybe Candidate
matchCands (Node x xl xr) (Node y yl yr) = guard (x `subsumes` y) >> (do
  l' <- matchCands xl yl
  r' <- matchCands xr yr
  return $ Node x l' r') <|> (Just . Leaf $ x)
matchCans l r = do
  guard (x `subsumes` y)
  return x
  where
    [x,y] = map extract [l,r]
  
  
    
