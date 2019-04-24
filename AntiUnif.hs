{-# LANGUAGE FlexibleInstances #-}
module AntiUnif where

import Types
import Sub

class AntiUnif a where
  antiUnif :: a -> a -> Maybe a

instance AntiUnif Clause where
  antiUnif = undefined
