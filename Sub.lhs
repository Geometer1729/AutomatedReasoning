
\begin{code}

module Sub where
import Types

\end{code}

A Sub, represents the assignment of a variable to some value.
Subs are stored as (ID,Term), where the ID indicates the Variable ID, and the Term indicates the value being substituded.
The Subable class represents anything to which a substitution can be applied.

\begin{code}
class Subable a where
	applySub :: Sub -> a -> a
\end{code}

The following generic instance allows a sub to be applied to a list if the elements in the list are subable
by applying the sub to each element of the list

\begin{code}
instance Subable a => Subable [a] where
	applySub sub = map $ applySub sub
\end{code}


