import UnifSub 
import Types
import ShowTex

cnf :: [[Predicate]]
cnf =  [ 
  [ P True  1 [Symbol 0 [] ] ] ,
  [ P False 1 [V 1] , P True  1 [Symbol 1 [V 1]]],
  [ P False 1 [V 1] , P False 1 [V 2]  , P True 1 [Symbol 2 [V 1,V 2]]]]

initial = initialize cnf

ls = iterate stepLayer initial

main = sequence . map (putStrLn . show) . take 5  $ ls
