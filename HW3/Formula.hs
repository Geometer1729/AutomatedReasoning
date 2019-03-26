module Formula where

import Text.ParserCombinators.ReadP
import Debug.Trace
{-
This module parses a formula in first order logic, and can rewrite with skolemization
-}

data Formula = Var String | 
               Function String [Formula] |
               And Formula Formula |
               Or Formula Formula |
               Not Formula |
               Forall String Formula |
               Exists String Formula
               deriving (Eq,Show)

testCase0 :: IO Formula
testCase0 = do
             let inp = "forall x. y and exists c. c or !x"
             f <- readFormula inp
             print f
             return f

testCase1 :: IO Formula
testCase1 = do
             let inp = "forall x. forall y. exists z. g(z,y) and forall w. exists u. u and h(w,u,x)"
             f <- readFormula inp
             print f
             return f

readC :: String -> [Clause]
readC s = let parser = readP_to_S parseLow
              parses = parser s
              finished = filter ((=="") . snd) parses
          in if length finished == 1
                then process $ fst . head $ finished
                else error "Ow my bones"

readFormula :: String -> IO Formula
readFormula s = do
                 let parser = readP_to_S parseLow
                 let parses = parser s
                 let finished = filter ((=="") . snd) parses
                 if length finished == 1
                     then return $ fst . head $ finished
                     else error $ "OW MY <i>BONES</i>: " ++ show finished

parseFormula :: ReadP Formula
parseFormula = parseForall +++ parseExists <++ parseVar +++ parseFunction +++ parseNot <++ parseOr <++ parseAnd 

parseLow :: ReadP Formula
parseLow = parseAnd +++ parseOr <++ parseQuant

parseQuant :: ReadP Formula
parseQuant = parseForall +++ parseExists <++ parseHigh

parseHigh :: ReadP Formula
parseHigh = (parseVar +++ parseFunction +++ parseNot)
            +++ (do
                  char '('
                  skipSpaces
                  f <- parseLow
                  skipSpaces
                  char ')'
                  return f)
            
parseVar :: ReadP Formula
parseVar = do
            s <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'])
            return $ Var s

parseFunction :: ReadP Formula
parseFunction = do
                 s <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'])
                 char '('
                 skipSpaces
                 args <- sepBy parseLow (char ',')
                 skipSpaces
                 char ')'
                 return $ Function s args

parseAnd :: ReadP Formula
parseAnd = do
            f1 <- parseHigh
            skipSpaces
            string "and"
            skipSpaces
            f2 <- parseLow
            return $ And f1 f2

parseOr :: ReadP Formula
parseOr = do
           f1 <- parseHigh
           skipSpaces
           string "or"
           skipSpaces
           f2 <- parseLow
           return $ Or f1 f2

parseNot :: ReadP Formula
parseNot = do
            (char '!') +++ (char '-')
            f <- parseHigh
            return $ Not f

parseForall :: ReadP Formula
parseForall = do
                string "forall"
                skipSpaces
                (Var s) <- parseVar
                char '.'
                skipSpaces
                f <- parseLow
                return $ Forall s f

parseExists :: ReadP Formula
parseExists = do
                string "exists"
                skipSpaces
                (Var s) <- parseVar
                char '.'
                skipSpaces
                f <- parseLow
                return $ Exists s f

{-- data Formula = Var String | 
               Function String [Formula] |
               And Formula Formula |
               Or Formula Formula |
               Not Formula |
               Forall String Formula |
               Exists String Formula
               deriving (Eq,Show)
 --}
data Term = V String | Symbol String [Term] | N Term deriving (Eq,Show)
type Clause = [Term]

process :: Formula -> [Clause]
process = (map . map $ formToTerm) . (map termList) . formList . skolemize -- . traceShowId 

skolemize :: Formula -> Formula
skolemize f = rewriteQuantifiers (nnf f) [] 0

formList :: Formula -> [Formula]
formList (And f1 f2) = f1 : formList f2
formList f = [f]

termList :: Formula -> [Formula]
termList (Or f1 f2) = f1 : termList f2
termList f = [f]

formToTerm :: Formula -> Term
formToTerm (Var s) = V s
formToTerm (Function s fs) = Symbol s (map formToTerm fs)
formToTerm (Not t) = N (formToTerm t)
formToTerm _ = error "Invalid type in term"

nnf :: Formula -> Formula
nnf (Var s) = Var s
nnf (Function s fs) = Function s (map nnf fs)
nnf (And f1 f2) = And (nnf f1) (nnf f2)
nnf (Or f1 f2) = Or (nnf f1) (nnf f2)
nnf (Not (And f1 f2)) = Or (Not (nnf f1)) (Not (nnf f2))
nnf (Not (Or f1 f2)) = And (Not (nnf f1)) (Not (nnf f2))
nnf (Not (Forall s f)) = Exists s (Not (nnf f))
nnf (Not (Exists s f)) = Forall s (Not (nnf f))
nnf (Not (Not f)) = nnf f
nnf (Not f) = Not (nnf f)
nnf (Forall s f) = Forall s (nnf f)
nnf (Exists s f) = Exists s (nnf f)

rewriteQuantifiers :: Formula -> [String] -> Int -> Formula
rewriteQuantifiers (Var s) _ _ = Var s
rewriteQuantifiers (Function s fs) vars i = Function s (map (\f -> rewriteQuantifiers f vars i) fs)
rewriteQuantifiers (And f1 f2) vars i = And (rewriteQuantifiers f1 vars i) (rewriteQuantifiers f2 vars i)
rewriteQuantifiers (Or f1 f2) vars i = Or (rewriteQuantifiers f1 vars i) (rewriteQuantifiers f2 vars i)
rewriteQuantifiers (Not f) vars i = Not (rewriteQuantifiers f vars i)
rewriteQuantifiers (Forall s f) vars i = rewriteQuantifiers f (s:vars) i
rewriteQuantifiers (Exists s f) vars i = rewriteQuantifiers (replaceExistVar f s vars i (length vars)) (s:vars) (succ i)

replaceExistVar :: Formula -> String -> [String] -> Int -> Int -> Formula
replaceExistVar (Var s) target vars i n | s /= target = Var s
                                        | otherwise = Function ("%f"++show i) $ take n [Var v | v <- vars]
replaceExistVar (Function s fs) target vars i n = Function s (map (\f -> replaceExistVar f target vars i n) fs)
replaceExistVar (And f1 f2) target vars i n = And (replaceExistVar f1 target vars i n) (replaceExistVar f2 target vars i n)
replaceExistVar (Or f1 f2) target vars i n = Or (replaceExistVar f1 target vars i n) (replaceExistVar f2 target vars i n)
replaceExistVar (Not f) target vars i n = Not (replaceExistVar f target vars i n)
replaceExistVar (Forall s f) target vars i n = Forall s (replaceExistVar f target (s:vars) i n)
replaceExistVar (Exists s f) target vars i n = Exists s (replaceExistVar f target vars i n)

--Ordering
instance Ord Term where
    (N t1) <= (N t2) = t1 <= t2
    t <= (N t1) = if t == t1 then True else t <= t1
    (N t1) <= t = if t == t1 then False else t1 <= t
    (V s1) <= (V s2) = s1 <= s2
    (V s1) <= (Symbol s ts) = True
    (Symbol s ts) <= (V s1) = False
    s@(Symbol f ss) <= t@(Symbol g ts) = case compare f g of
                                            GT -> False
                                            EQ -> lexic ss ts
                                            LT -> and [t > sx | sx <- ss] || or [tx > s | tx <- ts]

quicksort :: (Ord a, Eq a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = let l = [y | y <- xs, y < x]
                       g = [y | y <- xs, y > x]
                   in quicksort l ++ [x] ++ quicksort g

-- <
lexic :: (Ord a, Show a) => [a] -> [a] -> Bool
lexic m1 m2 = let s1 = quicksort m1
                  s2 = quicksort m2
              in lexicHelper s1 s2
              where
              lexicHelper :: (Ord a, Show a) => [a] -> [a] -> Bool
              lexicHelper [] _ = True
              lexicHelper _ [] = False
              lexicHelper (s1:xs) (s2:ys) = if s1 <= s2 then True else if s1 > s2 then False else lexicHelper xs ys
