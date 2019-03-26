{-# LANGUAGE LambdaCase #-}
module Main where

import Formula

evalImpl :: Formula -> Formula
evalImpl (And f1 f2) = And (evalImpl f1) (evalImpl f2)
evalImpl (Or f1 f2) = Or (evalImpl f1) (evalImpl f2)
evalImpl (Implies f1 f2) = Not (And (Not f2) f1)
evalImpl (Not f) = Not (evalImpl f)
evalImpl f = f

--assumed implication evaluated
nnf :: Formula -> Formula
nnf (Not (Or f1 f2)) = And (nnf (Not f1)) (nnf (Not f2))
nnf (Not (And f1 f2)) = Or (nnf (Not f1)) (nnf (Not f2))
nnf (Not (Not f)) = nnf f
nnf f = f

--assumes nnf
distribute :: Formula -> Formula
distribute (Or f1 (And f2 f3)) = And (distribute (Or f1 f2)) (distribute (Or f1 f3))
distribute (Or (And f2 f3) f1) = And (distribute (Or f1 f2)) (distribute (Or f1 f3))
distribute f = f

concatAnds :: [Formula] -> Formula
concatAnds [f] = f
concatAnds (f:fs) = And f (concatAnds fs)

cnf :: Formula -> Formula
cnf f = let ei = evalImpl f
            nf = nnf ei
        in distribute nf

--assumes cnf
clauseList :: Formula -> [Formula]
clauseList (And f1 f2) = clauseList f1 ++ clauseList f2
clauseList f = [f]

main :: IO ()
main = do
		g <- getLine
		let form = readUnsafe g
		let c = cnf form
		print $ clauseList c
