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
