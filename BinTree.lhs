\subsection{BinTree}

\begin{code}
module BinTree where
import Control.Comonad
\end{code}

This bin-tree module is written to support derivation histories.
For this reason each Node has exactly two children and the leaves which represent givens have no children.

\begin{code}
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a
\end{code}

The functor instance is pretty standard, the function is mapped over each element in the tree.

\begin{code}
instance Functor BinTree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
\end{code}

Much like the BinTree type can be viewed as a restriction of Tree to trees where each node has 0 or 2 children.
The comonad instance restricts the comonad instance for Tree.
Extract returns the root of the tree.
Duplicate replaces each point in the tree with the subtree of which it is the root.

\begin{code}
instance Comonad BinTree where
  extract (Node x _ _) = x
  extract (Leaf x )    = x
  duplicate t@(Leaf _) = Leaf t
  duplicate t@(Node _ l r)  = Node t (duplicate l) (duplicate r)
\end{code}

The Foldable instance is also pretty standard, It goes left children node right children

\begin{code}
instance Foldable BinTree where
  foldMap f (Leaf x) = f x
  foldMap f (Node x l r) = (foldMap f l) <> (f x) <> (foldMap f r)
\end{code}

This function gets all the leaves of the tree as a list.

\begin{code}
leaves :: BinTree a -> [a]
leaves (Leaf x) = [x]
leaves (Node _ l r) = leaves l ++ leaves r
\end{code}

The show instance shows the tree as structure with indentation putting each element on a different line.
The leaves are prefixed with G to denote givens, and the nodes with D to denote derived.

"init" is used twice after unlines because unlines puts a newline at the end which is desirable in neither case here.

\begin{code}
instance Show a => Show (BinTree a) where
  show (Leaf n) = "G " ++ show n
  show (Node n l r) = let
    indent = init . unlines . map ("  " ++) . lines
    in
    init . unlines $ [ "D " ++ (show n) ,  indent  $ show  l  , indent $ show r]
\end{code}
