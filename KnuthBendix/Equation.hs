module Equation where

import Text.ParserCombinators.ReadP

type Equation = (Term,Term)
data Term =  Var String
           | Function String [Term]
            deriving (Eq,Show)

test0 :: Term
test0 = readTerm "f(g(x),y)"

test1 :: Term
test1 = readTerm "_^^&(x y g(ROCK,FARM))"

test2 :: Equation
test2 = readEquation "f(x,x) = x"

test3 :: Equation
test3 = readEquation "x + y = y + x"

test4 :: Equation
test4 = readEquation "x + (y + z) = (x + y) + z"

test5 :: Equation
test5 = readEquation "x*(y+z)=x*y + x*z"

readRewriteSystem :: IO [Equation]
readRewriteSystem = do
                    s <- getLine
                    if null s
                        then return []
                        else do
                            let e = readEquation s
                            e' <- readRewriteSystem
                            return (e:e')

readEquation s = readParser s parseEquation
readTerm s = readParser s parseTerm

readParser :: String -> ReadP a -> a
readParser s p = let parser = readP_to_S p
                     parses = parser s
                     finished = filter ((=="") . snd) parses
                in if length finished == 1
                    then fst . head $ finished
                    else error $ "Number of parses: " ++ (show $ length finished)

parseEquation :: ReadP Equation
parseEquation = do
                t1 <- parseTerm
                skipSpaces
                char '='
                skipSpaces
                t2 <- parseTerm
                return (t1,t2)

parseTerm :: ReadP Term
parseTerm = parseFunction <++ parseOperator +++ parseVar
            <++ parseParen

parseParen :: ReadP Term
parseParen = do
             char '('
             t <- parseTerm
             char ')'
             return t

parseVar :: ReadP Term
parseVar = do
            id <- parseIdentifier
            return $ Var id

parseFunction :: ReadP Term
parseFunction = do
                id <- parseIdentifier
                char '('
                skipSpaces
                args <- sepBy parseTerm (char ',' +++ char ' ')
                skipSpaces
                char ')'
                return $ Function id args

parseIdentifier :: ReadP String
parseIdentifier = do
                    h <- satisfy (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_')
                    t <- munch (\c -> c `notElem` [' ','\n','\t','(',')',',','='] && c `notElem` operators)
                    return (h:t)

parseOperator :: ReadP Term
parseOperator = do
                t1 <- parseVar +++ parseFunction +++ parseParen
                skipSpaces
                op <- parseOpString
                skipSpaces
                t2 <- parseTerm
                return $ Function [op][t1,t2]

operators = ['+','-','*','/']

parseOpString :: ReadP Char
parseOpString = foldl (+++) pfail [char o | o <- operators]

instance Ord Term where
    (Var s1) <= (Var s2) = s1 <= s2
    (Var s1) <= (Function s2 args) = True
    (Function s2 args) <= (Var s1) = False
    f@(Function s1 args1) <= g@(Function s2 args2) = case compare s1 s2 of
                                                    GT -> False
                                                    EQ -> lexic args1 args2
                                                    LT -> and [g > a1 | a1 <- args1] || or [a2 > f | a2 <- args2]

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
