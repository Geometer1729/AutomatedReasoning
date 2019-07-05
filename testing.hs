import AntiUnif
import BinTree
import Implications
import ListUtil
import Nameable
import Namespace
import Parser
import Processing
import Renameable
import ShowTex
import Sub
import Subsume
import Tautology
import Types
import Unif

fromFile :: String -> IO (Namespace,Layer)
fromFile s = do
  cons <- readFile s
  let (pss,ns) = process . init $ cons
  let l = initialize pss
  return (ns,l)

loadTest :: Int -> IO (Namespace,Layer)
loadTest n = fromFile $ "tests/test" ++ show n
