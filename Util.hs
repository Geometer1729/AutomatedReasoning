module Util where

import Control.Monad.State
import Data.Tuple.Extra
import Types

liftState :: (b -> a) -> (b -> a ->b) -> State a c -> State b c
liftState geter seter s = do
  xb <- get
  let xa = geter xb
  let (r,ya) = runState s xa
  let yb = seter xb ya
  put yb
  return r

idGet :: Layer -> (ID,ID)
idGet = nextFreeClauseID &&& nextFreeVarID

idSet :: Layer -> (ID,ID) -> Layer
idSet l (c,v) = l{nextFreeClauseID = c,nextFreeVarID = v}

tableGet :: Layer -> Table
tableGet = table

tableSet :: Layer -> Table -> Layer
tableSet l t = l{table=t}

stateIDLift :: State (ID,ID) a -> State Layer a
stateIDLift = liftState idGet idSet

stateTableLift :: State Table a -> State Layer a
stateTableLift = liftState tableGet tableSet
