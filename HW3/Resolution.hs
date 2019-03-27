import Debug.Trace

import Data.Maybe

import Formula
import Unify


testResolution0 :: IO ()
testResolution0 = do
                  let i1 = "forall x. f(x) or !g(x,x)"
                  let i2 = "forall y. f(y) or g(y,y)"
                  let i3 = "forall z. !f(z)"
                  f1 <- readFormula i1
                  f2 <- readFormula i2
                  f3 <- readFormula i3
                  let c = process f1 ++ process f2 ++ process f3
                  print c
                  let ans = saturate c
                  print ans

testResolution1 :: IO ()
testResolution1 = do
                  let i1 = "exists z. nat(z)"
                  let i2 = "forall y. (!nat(y) and !nat(succ(y)))"
                  let i3 = "!(exists x. nat(succ(succ(succ(x)))))"
                  f1 <- readFormula i1
                  f2 <- readFormula i2
                  f3 <- readFormula i3
                  let c = process f1 ++ process f2 ++ process f3
                  print c
                  let ans = saturate c
                  print ans

testResolution2 :: IO ()
testResolution2 = do
                  let i1 = "exists z. nat(z)"
                  let i2 = "forall x. !(nat(x) and !nat(succ(x)))"
                  let i3 = "forall a. equal(a,a)"
                  let i4 = "exists three. exists two. equal(three,succ(two))"
                  f1 <- readFormula i1
                  f2 <- readFormula i2
                  f3 <- readFormula i3
                  f4 <- readFormula i4
                  let c = process f1 ++ process f2 ++ process f3 ++ process f4
                  print c
                  let ans = saturate c
                  print ans

testResolution3 :: IO ()
testResolution3 = do
                  f1 <- readFormula "f(x)"
                  f2 <- readFormula "!f(x)"
                  print $ saturate (process f1 ++ process f2)

test0 :: IO ()
test0 = do
        let c1 = readC "f(x)"
        let c2 = readC "!f(x)"
        let ans = saturate (c1++c2)
        print ans

testEvil :: IO ()
testEvil = do
  f1 <- readFormula "forall x. exists a. exists b. exists c. P(x,a) or P(c,b)"
  f2 <- readFormula "forall x. exists a. exists b. exists c. P(x,b) or !P(c,b)"
  f3 <- readFormula "forall x. exists a. exists b. exists c. !P(x,b) or P(c,a)"
  f4 <- readFormula "forall x. exists a. exists b. exists c. !P(x,a) or !P(c,a)"
  print $ saturate (concat . map process $ [f1,f2,f3,f4])

proveIO :: [String] -> String -> IO Bool
proveIO givens result = do
                    axioms <- mapM readFormula givens
                    goal <- readFormula result
                    let c = concat [process a | a <- axioms] ++ (fmap map map N $ process goal)
                    print c
                    return $ saturate c


readClauses :: IO [Clause]
readClauses = do
                s <- getLine
                if null s
                    then return []
                    else do
                        let c = readC s
                        cs <- readClauses
                        return (c++cs)

main :: IO ()
main = testEvil

  {-do
        print "Enter axioms line by line. Enter a blank line. Enter the goal clauses. Enter a blank line."
        axioms <- readClauses
        goals <- readClauses
        let negGoals = fmap map map N $ goals
        let proofs = [saturate $ ng:axioms | ng <- negGoals]
        print proofs
    -}


{-
data Term = V String | Symbol String [Term] deriving (Eq,Show)
type Clause = [Term]
-}

--Saturate a set of premises with resolution to check if empty clause is derived
saturate :: [Clause] -> Bool
saturate clauses = let cleaned = removeSubs . removeTaut . map quicksort $ clauses
                       new = {-trace ("Cleaned: " ++ show cleaned) $-} allResolutions cleaned  
                   in if {-trace ("New: " ++ show new)-} ([] `elem` new) then True else if null new then False else saturate (cleaned++new)

allResolutions :: [Clause] -> [Clause]
allResolutions [] = []
allResolutions (c:cs) = resolutions c cs ++ allResolutions cs

--Take a clause, and return all the resolutions that can be done with it and the second arg
resolutions :: Clause -> [Clause] -> [Clause]
resolutions l cs = catMaybes [applyResolution l c | c <- cs] 

--tries to unify the clauses and then resolve them
applyResolution :: Clause -> Clause -> Maybe Clause
applyResolution ((N t1):t1s) ((N t2):t2s) = Nothing
applyResolution ((N t1):t1s) (t2:t2s) = let u = unif t1 t2
                                        in if isFail u
                                            then Nothing
                                            else Just $ (map (applyUnification u) t1s) ++ (map (applyUnification u) t2s)

applyResolution (t1:t1s) ((N t2):t2s) = let u = unif t1 t2
                                        in if isFail u
                                            then Nothing
                                            else Just $ (map (applyUnification u) t1s) ++ (map (applyUnification u) t2s)
applyResolution _ _ = Nothing

--Removes tautologies and also factors
removeTaut :: [Clause] -> [Clause]
removeTaut clauses = filter (not . taut) clauses
                     where taut :: Clause -> Bool
                           taut [] = False
                           taut (t:ts) = t `isIn` ts || taut ts

isIn :: Term -> Clause -> Bool                                  
isIn s ts = or . map isU $ [unify [(s,case t of (N v) -> v; v -> v)] | t <- ts]

isIn' :: Term -> Clause -> Bool                                  
isIn' s ts = or . map isU $ [unify [(s,t)] | t <- ts]

removeSubs :: [Clause] -> [Clause]
removeSubs clauses = {-trace ("Subsumed: " ++ show [s | s <- clauses, isSubs s])-} filter (not . isSubs) clauses
                     where isSubs :: Clause -> Bool
                           isSubs c = or [subs cx c && cx /= c | cx <- clauses]

subs :: Clause -> Clause -> Bool
subs c1 c2 = c1 < c2 && and [c `isIn'` c2 | c <- c1]
