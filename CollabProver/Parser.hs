module Parser where

import Text.ParserCombinators.ReadP
import Debug.Trace

import Types

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

readFormula :: String -> Formula
readFormula s = let parser = readP_to_S parseLowPrec
                    parses = parser s
                    finished = filter ((=="") . snd) $ traceShowId parses
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
