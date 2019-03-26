import Clause

literalLEQ :: Literal -> Literal -> Bool
literalLEQ (P s0) (P s1) = s0 <= s1
literalLEQ (P s0) (N s1) = s0 < s1
literalLEQ (N s0) (P s1) = s0 < s1
literalLEQ (N s0) (N s1) = s0 <= s1

instance Ord Literal where
    (<=) = literalLEQ

--test clauses
x :: [Clause]
x = [[P "p",P "q",P "r"],[N "p",P "q",P "r"]]
y :: [Clause]
y = [[P "r",N "q",P "p"],[P "r",P "q",P "p"]]

--quicksort that removes duplicates
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = let l = [y | y <- xs, y < x]
                       g = [y | y <- xs, y > x]
                   in quicksort g ++ [x] ++ quicksort l

--remove tautologies
removeTaut :: [Clause] -> [Clause]
removeTaut cs = filter (not . taut) cs
                where taut :: Clause -> Bool
                      taut [] = False
                      taut (l:ls) = (neg l) `elem` ls || taut ls

implies :: Clause -> Clause -> Bool
implies c1 c2 = and [x `elem` c2 | x <- c1]

subs :: Clause -> Clause -> Bool
subs c1 c2 = c1 < c2 && implies c1 c2

removeSubs :: [Clause] -> [Clause]
removeSubs cs = let subsumedClauses = [c | c <- cs, or [subs d c | d <- cs]]
                in [c | c <- cs, c `notElem` subsumedClauses]

--assumes sorted lists
resolution :: [Clause] -> [Clause]
resolution cs = let lits = [(head c,tail c) | c <- cs]
                in concat [resolve l lits | l <- lits]
                where resolve :: (Literal,Clause) -> [(Literal,Clause)] -> [Clause]
                      resolve (l,c) ds = [snd d | d <- ds, neg l == fst d]

saturate :: [Clause] -> [Clause]
saturate cs = let cleaned = removeSubs . removeTaut . map quicksort $ cs
                  rs = if [] `elem` cleaned then [] else resolution cleaned
              in if null rs then cleaned else saturate $ rs++cleaned

main :: IO ()
main = do
        cs <- readFormula
        let sats = saturate cs
        if [] `elem` sats
            then print "UNSAT"
            else print "SAT: " >>= \_ -> print sats
