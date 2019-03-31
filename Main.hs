import System.Environment
import Types
import Parser
import ShowTex
import Processing
import Dnf

main = do
  args <- getArgs 
  cons <- readFile . head $ args 
  putStrLn . handle . init $ cons

handle :: String -> String
handle s = (unlines $ map (flip nsshow ns) pss) ++ (unlines $ map (flip nsshow ns) djs) ++ "\n" ++ (unlines $ map (flip nsshow ns) sdjs)
  where
    (pss,ns) = process s 
    djs = dnfify pss
    sdjs = reduce djs

