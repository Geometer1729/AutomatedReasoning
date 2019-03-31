{-# LANGUAGE FlexibleInstances #-}

module ShowTex where
import Types
import Text.Show

class TexShow a where
  texshow :: a -> String

instance {-# OVERLAPPING #-} Show Clause where
  show ([],[]) = "|"
  show (l,[]) = show l ++ " |"
  show ([],r) = "| " ++ show r
  show (l,r) = show l ++ " | " ++ show r

instance Show Predicate where
  show (P True id []) = "P" ++ show id
  show (P True id lt) = "P" ++ show id ++ "(" ++ show lt ++ ")"
  show (P False id []) = "~P" ++ show id
  show (P False id lt) = "~P" ++ show id ++ "(" ++ show lt ++ ")"

instance {-# OVERLAPPING #-} Show [Predicate] where
  show [p] = show p
  show (p:l) = show p ++ ", " ++ show l
  show [] = ""

instance Show Term where
  show (V id) = "X" ++ show id
  show (Symbol id []) = "a" ++ show id
  show (Symbol id tl) = "f" ++ show id ++ "(" ++ show tl ++ ")"

instance {-# OVERLAPPING #-} Show [Term] where
  show [t] = show t
  show (t:l) = show t ++ ", " ++ show l
  show [] = error "Attempt to show an empty list of terms"

  -- TODO
--instance TexShow Layer where

instance TexShow Clause where
  texshow ([],[]) = "$|$"
  texshow (l,[]) = "$" ++  show l ++ " |$"
  texshow ([],r) = "| " ++ show r
  texshow (l,r) = "$" ++ show l ++ " | " ++ show r ++ "$"

instance TexShow Predicate where
  texshow (P True id []) = "P_{" ++ show id ++ "}"
  texshow (P True id lt) = "P_{" ++ show id ++ "}(" ++ show lt ++ ")"
  texshow (P False id []) = "~P_{" ++ show id ++ "}"
  texshow (P False id lt) = "~P_{" ++ show id ++ "}(" ++ show lt ++ ")"

instance TexShow [Predicate] where
  texshow [p] = show p
  texshow (p:l) = show p ++ ", " ++ show l
  texshow [] = ""

instance TexShow Term where
  texshow (V id) = "X_{" ++ show id ++ "}"
  texshow (Symbol id []) = "a_{" ++ show id ++ "}"
  texshow (Symbol id tl) = "f_{" ++ show id ++ "}(" ++ show tl ++ ")"

instance TexShow [Term] where
  texshow [t] = show t
  texshow (t:l) = show t ++ ", " ++ show l
  texshow [] = error "Attempt to texshow an empty list of terms"
