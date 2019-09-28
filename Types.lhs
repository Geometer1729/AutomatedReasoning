\subsection{Types}

\begin{code}
module Types where

import BinTree
import Data.List
import qualified Data.Map as M
\end{code}

\subsubsection{Types Used During Parsing}

A Formula is Either a variable with a name a function one of the logical operators or a Quantified formula.
At this stage both Predicates and Functions are stored as functions, and Both Variables are stored as Var.
Show is Derived for Debugging purposes.

\begin{code}
data Formula = Var String
              |Function String [Formula]
              |And Formula Formula
              |Or Formula Formula
              |Not Formula
              |Implies Formula Formula
              |Quantified Quantifier Formula
              deriving (Eq,Show)

data Quantifier = Forall String | Exists String deriving (Eq,Show)
\end{code}

\subsubsection{Types Used in the Prover}

The types used in the prover differentiate predicates from functions and constants from variables. 
They also use ID, (which is a type alias for Int) rather than Strings to represent names.
This should improve performance, but also makes it easy to systematically generate new names.
\begin{code}
type ID = Int
\end{code}


Predicates represent a possibly negated predicate, and all its arguments.
The Bool represents rather the predicate is negated, the ID, is

\begin{code}
data Predicate = P Bool ID [Term] deriving (Eq,Ord)
\end{code}

Terms are either variables, constants or functions.
Inside the prover constants are treated as nularry functions.
Variables only store their ID. Functions store their ID and 

\begin{code}
data Term = V ID | Symbol ID [Term] deriving (Eq,Ord)
\end{code}

To keep the regular EQ instance as true equality the ordering used for ordered resolution is not the Ord Instance.

\begin{code}
termPo :: Term -> Term -> Ordering
termPo (V _) _ = EQ
termPo _ (V _) = EQ
termPo (Symbol li lts) (Symbol ri rts) = case compare li ri of
  LT -> LT
  GT -> GT 
  EQ -> compare lts rts
\end{code}



