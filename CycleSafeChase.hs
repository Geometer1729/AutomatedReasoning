module CycleSafeChase where

import Data.Maybe
import Control.Monad
import Control.Monad.State

cycleFail :: (Eq a) => (a -> Either a (Maybe b)) -> a -> Maybe b
cycleFail f x = evalState (stateChase f x) []
  
stateChase :: (Eq a) => (a -> (Either a (Maybe b))) -> a -> State [a] (Maybe b)
stateChase f x = do
  case f x of
    Right b -> return b
    Left a -> do
      ys <- get
      let cycle = or $ [ a == y | y <- ys ]
      modify (a:)
      if cycle then return Nothing else stateChase f a

