import System.Environment
import Types
import Parser
import ShowTex
import Processing

main = do
  args <- getArgs 
  cons <- readFile . head $ args 
  let n = read $ args !! 1
  putStrLn . handle n . init $ cons

handle :: Int -> String -> String
handle n s = unlines . map (flip nsshow ns) $ ls
  where
    (pss,ns) = process s 
    l = initialize pss :: Layer
    ls = take n $ iterate stepLayer l :: [Layer]

