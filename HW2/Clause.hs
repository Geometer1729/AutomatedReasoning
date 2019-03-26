module Clause where

import Text.ParserCombinators.ReadP
import Data.Char

data Literal = P String | N String deriving (Show,Eq)
type Clause = [Literal]

neg :: Literal -> Literal
neg (P s) = N s
neg (N s) = P s

readFormula :: IO [Clause]
readFormula = do
                s <- getLine
                if null s
                    then return []
                    else do
                        let c = readClause s
                        cs <- readFormula
                        return (c:cs)

readClause :: String -> Clause
readClause s = let parses = (readP_to_S parseClause) [c | c <- s, c /= ' ']
                   finished = filter ((=="") . snd) parses
               in if length finished == 1 then fst . head $ finished else error $ "Multiple parses for: " ++ s

parseLiteral :: ReadP Literal
parseLiteral = (do
                 text <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
                 return $ P text)
                +++
                (do
                 char '-'
                 text <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
                 return $ N text)
                +++
                (do
                 char '!'
                 text <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
                 return $ N text)

parseClause :: ReadP Clause
parseClause = do
                lit <- parseLiteral
                lits <- many (do --zero or more
                                char ','
                                l <- parseLiteral
                                return l)
                char '.'
                return $ lit:lits
