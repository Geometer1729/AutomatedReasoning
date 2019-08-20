{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where

import Text.ParserCombinators.ReadP
import Debug.Trace

import Types
import Namespace

{-
data Formula = Var String
              |Function String [Formula]
              |And [Formula]
              |Or [Formula]
              |Not Formula
              |Implies Formula Formula
              |Quantified Quantifier Formula
              deriving (Eq,Show)

data Quantifier = Forall String | Exists String deriving (Eq,Show)
-}


process :: String -> ([[Predicate]],Namespace)
process s = let f = readFormula s
                skl = skolemize f
                ns = empty
            in formList skl ns

skolemize :: Formula -> Formula
skolemize f = rewrQuants (nnf f) [] 0

--data Predicate = P Bool ID [Term] deriving(Eq,Ord,Show)

formList :: Formula -> Namespace -> ([[Predicate]],Namespace)              
formList (And f1 f2) ns = let (h,ns') = termList f1 ns
                              (t,ns'') = formList f2 ns'
                          in (h:t,ns'')
formList f ns = let (t,ns') = termList f ns
                in ([t],ns')

termList :: Formula -> Namespace -> ([Predicate],Namespace)              
termList (Or (Not f1) f2) ns = let (p,ns') = predicate f1 False ns
                                   (predTail,ns'') = termList f2 ns'
                               in (p:predTail,ns'')
termList (Or f1 f2) ns = let (p,ns') = predicate f1 True ns
                             (predTail,ns'') = termList f2 ns'
                         in (p:predTail,ns'')
termList (Not f) ns = let (p,ns') = predicate f False ns
                      in ([p],ns')
termList f ns = let (p,ns') = predicate f True ns
                in ([p],ns')

predicate :: Formula -> Bool -> Namespace -> (Predicate,Namespace)
predicate f@(Function s _) b ns = let (i,ns') = insert ns s
                                      (Symbol _ args,ns'') = formToTerm f ns'
                                   in (P b i args,ns'')
predicate _ _ _ = error "inapropriate call to predicate in parser"

formToTerm :: Formula -> Namespace -> (Term,Namespace)
formToTerm (Var s) ns = let (i,ns') = insert ns s
                        in (V i,ns')
formToTerm (Function s fs) ns = let (i,ns') = insert ns s
                                    listTerms :: [Formula] -> Namespace -> ([Term],Namespace)
                                    listTerms [] n = ([],n)
                                    listTerms (f:ft) n = let (t,n') = formToTerm f n
                                                             (termTail,n'') = listTerms ft n'
                                                         in (t:termTail,n'')
                                    (args,finalNs) = listTerms fs ns'
                                in (Symbol i args, finalNs)
formToTerm _ _ = error "inapropriate call to formToTerm in parser"

nnf :: Formula -> Formula
nnf (Var s) = (Var s)
nnf (Function s args) = Function s (map nnf args)
nnf (And f1 f2) = And (nnf f1) (nnf f2)
nnf (Or f1 f2) = Or (nnf f1) (nnf f2)
nnf (Not (And f1 f2)) = Or (Not $ nnf f1) (Not $ nnf f2) 
nnf (Not (Or f1 f2)) = And (Not $ nnf f1) (Not $ nnf f2)
nnf (Not (Implies f1 f2)) = And (nnf f1) (Not $ nnf f2)
nnf (Not (Not f)) = nnf f
nnf (Not (Quantified (Forall s) f)) = Quantified (Exists s) (Not $ nnf f)
nnf (Not (Quantified (Exists s) f)) = Quantified (Forall s) (Not $ nnf f)
nnf (Not f) = Not $ nnf f
nnf (Implies f1 f2) = Or (Not $ nnf f1) (nnf f2) --Rewrites implication
nnf (Quantified (Forall s) f) = Quantified (Forall s) (nnf f)
nnf (Quantified (Exists s) f) = Quantified (Exists s) (nnf f)

--Assumes implication is rewritten
--vars is the list of variables bound at this point in the tree
--i is how many new names we have added
rewrQuants :: Formula -> [String] -> Int -> Formula
rewrQuants (Var s) _ _ = Var s
rewrQuants (Function s fs) vars i = Function s [rewrQuants f vars i | f <- fs]
rewrQuants (And f1 f2) vars i = And (rewrQuants f1 vars i) (rewrQuants f2 vars i) 
rewrQuants (Or f1 f2) vars i = Or (rewrQuants f1 vars i) (rewrQuants f2 vars i)
rewrQuants (Not f) vars i = Not $ rewrQuants f vars i
rewrQuants (Quantified (Forall s) f) vars i = rewrQuants f (s:vars) i
rewrQuants (Quantified (Exists s) f) vars i = rewrQuants (replExist f s vars i (length vars)) (s:vars) (succ i)
rewrQuants (Implies _ _) _ _ = error "implication passed to rewrQuants"

replExist :: Formula -> String -> [String] -> Int -> Int -> Formula
replExist (Var s) target vars i n | s /= target = Var s
                                  | otherwise = Function ("%f"++show i) $ take n [Var v | v <- vars]
replExist (Function s args) target vars i n = Function s (map (\f -> replExist f target vars i n) args)
replExist (And f1 _) target vars i n = And (replExist f1 target vars i n) (replExist f1 target vars i n)
replExist (Or f1 _) target vars i n = Or (replExist f1 target vars i n) (replExist f1 target vars i n)
replExist (Not f) target vars i n = Not $ replExist f target vars i n
replExist (Quantified (Forall s) f) target vars i n = Quantified (Forall s) $ replExist f target (s:vars) i n
replExist (Quantified (Exists s) f) target vars i n = Quantified (Exists s) $ replExist f target vars i n
replExist (Implies _ _) _ _ _ _ = error "implication passed to replExist"


readFormula :: String -> Formula
readFormula s = let parser = readP_to_S parseLowPrec
                    parses = parser s
                    finished = filter ((=="") . snd) parses
                in if length finished == 1
                    then fst . head $ finished
                    else traceShow parses $ error "BAD PARSE"

parseHighPrec :: ReadP Formula
parseHighPrec = parseVar +++ parseFunction +++ parseNot +++ (do
                char '('
                skipSpaces
                f <- parseLowPrec
                skipSpaces
                char ')'
                return f)

parseLowPrec :: ReadP Formula
parseLowPrec = parseImplies +++ parseAnd +++ parseOr <++ parseMedPrec

parseMedPrec :: ReadP Formula
parseMedPrec =  (do
                q <- parseForall +++ parseExists
                a <- parseLowPrec
                return $ Quantified q a) <++ parseHighPrec


parseVar :: ReadP Formula
parseVar = do
            h <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
            t <- munch (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'])
            return $ Var (h++t)

parseFunction :: ReadP Formula
parseFunction = do
                    h <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
                    t <- munch (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'])
                    char '('
                    skipSpaces
                    args <- sepBy parseLowPrec (char ',')
                    skipSpaces
                    char ')'
                    return $ Function (h++t) args

parseAnd :: ReadP Formula
parseAnd = do
            f1 <- parseHighPrec
            skipSpaces
            (string "and" +++ string "&&")
            skipSpaces
            f2 <- parseLowPrec
            return $ And f1 f2

parseOr :: ReadP Formula
parseOr = do 
            f1 <- parseHighPrec
            skipSpaces
            (string "or" +++ string "||")
            skipSpaces
            f2 <- parseLowPrec
            return $ Or f1 f2


parseNot :: ReadP Formula
parseNot = do
            (char '!') +++ (char '~') +++ (char '-')
            f <- parseHighPrec
            return $ Not f

parseImplies :: ReadP Formula
parseImplies = do
                f1 <- parseHighPrec
                skipSpaces
                string "->"
                skipSpaces
                f2 <- parseLowPrec
                return $ Implies f1 f2

parseForall :: ReadP Quantifier
parseForall = do
                string "forall"
                skipSpaces
                (Var s) <- parseVar
                char '.'
                skipSpaces
                return $ Forall s

parseExists :: ReadP Quantifier
parseExists = do
                string "exists"
                skipSpaces
                (Var s) <- parseVar
                char '.'
                skipSpaces
                return $ Exists s
