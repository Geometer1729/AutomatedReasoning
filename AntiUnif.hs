{-# LANGUAGE FlexibleInstances #-}
module AntiUnif where

import Types
import Sub

class AntiUnif a where
  antiUnif :: a -> a -> Maybe (

instance AntiUnif Term where
  antiUnif (V i) (Symbol _ _)

instance AntiUnif Clause where
  antiUnif = undefined
