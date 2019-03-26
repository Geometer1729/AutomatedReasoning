module Formula where

import Text.ParserCombinators.ReadP
import Data.Char
import Debug.Trace

data Formula = Var String
			 | T
			 | F
			 | And Formula Formula
			 | Or Formula Formula
			 | Not Formula
			 | Implies Formula Formula
			 -- | Equal Formula Formula --Are we doing this yet?
			deriving (Eq,Show)

readUnsafe :: String -> Formula
readUnsafe s = case readFormula s of
				Nothing -> error "ow"
				(Just f) -> f

readFormula :: String -> Maybe Formula
readFormula s = let parses = (readP_to_S parseOp) s
                    finished =  [p | p <- parses, snd p == ""]
				in if length finished == 1 then Just (fst . head $ finished) else Nothing

parseVar :: ReadP Formula
parseVar = do
			start <- munch1 (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
			rest <- munch (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'])
			if (start++rest) `notElem` ["true","false"] then return $ Var (start++rest) else pfail

parseOp :: ReadP Formula
parseOp =  (do
			 char '!'
			 c <- parseOp
			 return $ Not c)
		   +++
		   (do
			f1 <- parseLow
			skipSpaces
			string "and"
			skipSpaces
			f2 <- parseOp
			return $ And f1 f2)
		  +++
		  (do
		  	f1 <- parseLow
			skipSpaces
			string "or"
			skipSpaces
			f2 <- parseOp
			return $ Or f1 f2)
		  +++
		  (do
		  	f1 <- parseLow
			skipSpaces
			string "->"
			skipSpaces
			f2 <- parseOp
			return $ Implies f1 f2)
		  <++ parseLow

--low precedence parsing
parseLow :: ReadP Formula 
parseLow = (do
			string "true"
			return T)
			+++
			(do
			string "false"
			return F)
			<++
			parseVar
			<++
			(do
			char '('
			skipSpaces
			f <- parseOp
			skipSpaces
			char ')'
			return f)
