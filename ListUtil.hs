module ListUtil where

getLeftPairs :: ([a],[a]) -> [(a,[a])]
-- gets every pair of one element in the left and all other elements left or right
getLeftPairs (ls,rs) = [ (a,xs++rs) | (a,xs) <- getEntsWithRest ls ]

getEntsWithRest :: [a] -> [(a,[a])]
-- gets every pair of one element and all other elements
getEntsWithRest [] = []
getEntsWithRest (x:xs) = (x,xs) : [ (a,x:b) | (a,b) <- getEntsWithRest xs ]

selfPairs :: [a] -> [(a,a)]
-- returns every pair of one element from the list and a element not before it from the list
selfPairs [] = []
selfPairs (x:xs) = (x:x) : [ (x,x') | x' <- xs ] ++ (selfPairs xs)

resTargets :: [a] -> [a] -> [(a,a)]
-- returns every pair of one element from the left and one from the right
-- and every pair of one element from the right and a subsequent element from the right
resTargets ls rs = [ (l,r) | l <- ls , r <- rs ] ++ (selfPairs rs)
