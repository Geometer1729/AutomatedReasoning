module BinTree where
import Control.Comonad


data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a

instance Functor BinTree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Comonad BinTree where
  extract (Node x _ _) = x
  extract (Leaf x )    = x
  duplicate t@(Leaf _) = Leaf t
  duplicate t@(Node _ l r)  = Node t (duplicate l) (duplicate r)

toList :: BinTree a -> [a]
toList (Leaf x) = [x]
toList (Node x l r) = (toList l) ++ [x] ++ (toList r)

leaves :: BinTree a -> [a]
leaves (Leaf x) = [x]
leaves (Node _ l r) = leaves l ++ leaves r

instance Show a => Show (BinTree a) where
  show (Leaf n) = "G " ++ show n
  show (Node n h1 h2) = "D " ++ (show n) ++ "\n" ++  (indent . show $ h1) ++ "\n" ++  (indent . show $ h2)
    where
      indent = init . unlines . map ("  " ++) . lines
