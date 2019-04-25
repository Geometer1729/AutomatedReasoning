module Implications where

import Types
import Subsume
import qualified Data.Map as M
import Data.Maybe
import BinTree
import AntiUnif
import Control.Monad
import Control.Applicative
import Control.Comonad

getCandidates :: (M.Map ID Clause) -> History -> [Candidate] 
getCandidates clauses h = catMaybes [ matchCands st tree | st <- subTrees ]
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
matchCans l r = guard (x `subsumes` y) >> Just x
  where
    [x,y] = map extract [l,r]


makeImp :: Candidate -> Maybe Implication
makeImp cand = do
  let hs = leaves cand
  let c = extract cand
  -- the conclusion is longer than some hypothesis 
  guard $ or  [ (length h) <= (length c) | h <- hs ] 
  -- some hypothesis subsumes the conclusion
  guard $ or [h `subsumes` c | h <- hs ]
  return (hs,c)

genImps :: (M.Map ID Clause) -> [History] -> [Implication]
genImps clauses = catMaybes . concat . map (map makeImp . getCandidates clauses)
