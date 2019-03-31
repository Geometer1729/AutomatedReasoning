import System.Environment
import Types
import Parser
import ShowTex
import Processing

main = do
  args <- getArgs 
  cons <- readFile . head $ args 
  let n = if length args > 1 then Just . read $ args !! 1 else Nothing
  putStrLn . handle n . init $ cons

handle :: Maybe Int -> String -> String
handle mn s = unlines . map (flip nsshow ns) $ ls
  where
    (pss,ns) = process s 
    l = initialize pss :: Layer
    ls = case mn of 
      (Just n) -> take n $ iterate stepLayer l 
      Nothing -> iterate stepLayer l 

