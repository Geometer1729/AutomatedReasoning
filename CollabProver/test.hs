import UnifSub 
import Types

cnf :: [[Predicate]]
cnf =  [ [ P True  1 [Symbol 0 [] ] ] , [ P False 1 [V 1] , P True 1 [Symbol 1 [V 1]]] ]

initial = initialize cnf

ls = iterate stepLayer initial

main = sequence . map print . take 10  $ ls
