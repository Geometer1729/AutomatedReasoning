module Main where

import Formula
import Debug.Trace

getVars :: Formula -> [String]
getVars (Var s) = [s]
getVars T = []
getVars F = []
getVars (Not f) = getVars f
getVars (And f f') = let varf = getVars f
                         varf' = getVars f'
                     in varf ++ [v | v <- varf', v `notElem` varf]
getVars (Or f f') = let varf = getVars f
                        varf' = getVars f'
                    in varf ++ [v | v <- varf', v `notElem` varf]
getVars (Implies f f') = let varf = getVars f
                             varf' = getVars f'
                         in varf ++ [v | v <- varf', v `notElem` varf]

truthAssignment :: Formula -> (Formula,Formula, String)
truthAssignment f = let name = head $ getVars f
                        var = Var name
                    in (replace var T f,replace var F f, name)

--first formula MUST be a Var
replace :: Formula -> Formula -> Formula -> Formula
replace (Var s) new (Var g) = if g == s then new else (Var g)
replace (Var s) new T = T
replace (Var s) new F = F
replace (Var s) new (And f1 f2) = And (replace (Var s) new f1) (replace (Var s) new f2) 
replace (Var s) new (Or f1 f2) = Or (replace (Var s) new f1) (replace (Var s) new f2) 
replace (Var s) new (Not f) = Not $ replace (Var s) new f
replace (Var s) new (Implies f1 f2) = Implies (replace (Var s) new f1) (replace (Var s) new f2) 

reduce :: Formula -> Formula
reduce (Var s) = Var s
reduce T = T
reduce F = F
reduce (And T f) = reduce f
reduce (And f T) = reduce f
reduce (And F f) = F
reduce (And f F) = F
reduce (And f1 f2) = And (reduce f1) (reduce f2)
reduce (Or T f) = T
reduce (Or f T) = T
reduce (Or F f) = reduce f
reduce (Or f F) = reduce f
reduce (Or f1 f2) = Or (reduce f1) (reduce f2)
reduce (Not T) = F
reduce (Not F) = T
reduce (Not f) = Not $ reduce f
reduce (Implies F f) = T
reduce (Implies f T) = T
reduce (Implies f1 f2) = Implies (reduce f1) (reduce f2)

evaluate :: Formula -> Bool
evaluate (Implies T F) = False
evaluate T = True
evaluate F = False
evaluate f = let r = reduce f in if f == r then error "free variable in formula" else evaluate r

data TA = TR String | FA String | UN String deriving Show
data Result = SAT [TA] | UNSAT deriving Show

isSAT :: Result -> Bool
isSAT (SAT _) = True
isSAT _ = False

satisfiable :: Formula -> [TA] -> Result
satisfiable f l = let vars = getVars f
                  in if null vars then traceShow f $ if evaluate f then SAT l else UNSAT
                     else let (ta,fa,v) = truthAssignment f
                              tRes = satisfiable (reduce ta) (TR v : l)
                              fRes = satisfiable (reduce fa) (FA v : l)
                          in if isSAT tRes then tRes else if isSAT fRes then fRes else UNSAT


main :: IO ()
main = do
        s <- getLine
        let testform = case readFormula s of 
                        (Just s) -> s
                        Nothing -> error "wtf"
        print $ satisfiable testform []







































