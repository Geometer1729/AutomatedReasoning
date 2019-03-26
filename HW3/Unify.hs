module Unify where

import Data.Typeable
import Debug.Trace

import Control.Monad
import Data.Monoid
import Formula

{-
data Term = V String | Symbol String [Term] deriving (Eq,Show)
type Clause = [Term]
-}

data Unifier = U [(String,Term)] | F deriving (Eq,Show)

isFail :: Unifier -> Bool
isFail F = True
isFail _ = False

isU :: Unifier -> Bool
isU F = False
isU _ = True

instance Monoid Unifier where
    mempty = U []
    mappend u1 F = F
    mappend F u2 = F
    mappend (U sub1) (U sub2) = U (sub1 ++ sub2)

applyUnification :: Unifier -> Term -> Term
applyUnification F _ = error "Cannot apply a failed unification!"
applyUnification (U []) t = t
applyUnification (U ((s,e):ss)) t = applyUnification (U ss) (substitute t s e)

substitute :: Term -> String -> Term -> Term
substitute (V s) target sub | s == target = sub
                            | otherwise = V s
substitute (Symbol s ts) target sub = Symbol s [substitute t target sub | t <- ts]
substitute (N s) target sub = N $ substitute s target sub

unif :: Term -> Term -> Unifier
unif t1 t2 = unify [(t1,t2)]

unify :: [(Term,Term)] -> Unifier
unify eqs = let delete = filter (uncurry (/=)) eqs
            in case delete of
                ((Symbol s1 t1s,Symbol s2 t2s):e) -> if s1 /= s2 then F else unify (zip t1s t2s) <> unify e
                ((Symbol s ts,V x):e) -> U [(x,Symbol s ts)] <> unify (applySubToEqList x (Symbol s ts) e)
                ((V x,s):e) -> U [(x,s)] <> unify (applySubToEqList x s e)
                [] -> mempty
                ((N t,N s):e) -> unify $ (t,s):e
                ((N t,s):e) -> F
                ((t,N s):e) -> F
                s -> error $ show s ++ ": What?"

applySubToEqList :: String -> Term -> [(Term,Term)] -> [(Term,Term)]
applySubToEqList target sub eqs = [(substitute e1 target sub, substitute e2 target sub) | (e1,e2) <- eqs]

testCase1 :: IO ()
testCase1 = do
            let i1 = "forall x. exists y. g(y,h(y)) or !(h(x,y) and p(x,x))"
            r <- readFormula i1
            let f = process r
            print f

testUnify0 :: IO ()
testUnify0 = do
             f1 <- readFormula "exists a. forall x. f(a,x) or h(x)"
             f2 <- readFormula "forall y. forall x. f(x,y) or h(y)"
             let p1 = process f1
             let p2 = process f2
             print $ typeOf p1
             print $ typeOf p2
             let eqs = liftM2 zip p1 p2
             print $ typeOf eqs
             print eqs
             let unifs = map unify eqs
             print $ typeOf unifs
             print unifs
