{-# LANGUAGE FlexibleInstances #-}

module ShowTex where
import Types
import Namespace
import Text.Show
import BinTree

class NsShow a where
  nsshow :: a -> Namespace -> String

class TexShow a where
  texshow :: a -> Namespace -> String

idshow :: ID -> Namespace -> SymbolType -> String
idshow id ns tt = case unmap ns id of
  Just x -> x
  Nothing -> case tt of
    SymbolPredicate -> "P" ++ show id
    SymbolFunction -> "f" ++ show id
    SymbolNilary -> "a" ++ show id
    SymbolVariable -> "X" ++ show id

instance NsShow Predicate where
  nsshow (P True id []) ns = idshow id ns SymbolPredicate
  nsshow (P True id lt) ns = (idshow id ns SymbolPredicate) ++ "(" ++ (nsshow lt ns) ++ ")"
  nsshow (P False id []) ns = "~" ++ idshow id ns SymbolPredicate
  nsshow (P False id lt) ns = "~" ++ (idshow id ns SymbolPredicate) ++ "(" ++ (nsshow lt ns) ++ ")"

instance {-# OVERLAPPING #-} NsShow [Predicate] where
  nsshow [p] ns = (nsshow p ns)
  nsshow (p:l) ns = (nsshow p ns) ++ ", " ++ nsshow l ns
  nsshow [] ns = ""

instance NsShow Term where
  nsshow (V id) ns = idshow id ns SymbolVariable
  nsshow (Symbol id []) ns = idshow id ns SymbolNilary
  nsshow (Symbol id tl) ns = (idshow id ns SymbolFunction) ++ "(" ++ (nsshow tl ns) ++ ")"

instance {-# OVERLAPPING #-} NsShow [Term] where
  nsshow [t] ns = nsshow t ns
  nsshow (t:l) ns = (nsshow t ns) ++ ", " ++ nsshow l ns
  nsshow [] ns = error "Attempt to nsshow an empty list of terms"

instance NsShow Layer where
  nsshow l ns = unlines $ "begin layer\nProcesed" : (map chShow ps) ++ ["Unprocessed"] ++ (map chShow us) ++ ["end layer"]
    where
      ps = processedClauses l 
      us = unprocessedClauses l
      chShow :: (Clause,History) -> String
      chShow (c,h) = unlines [nsshow c ns]

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

instance Show History where
  show (Leaf n) = "G " ++ show n
  show (Node n h1 h2) = "D " ++ (show n) ++ "\n" ++  (indent . show $ h1) ++ "\n" ++  (indent . show $ h2)
    where
      indent = init . unlines . map ("  " ++) . lines

instance Show Layer where
  show l = unlines $ "begin layer\nProcesed" : (map chShow ps) ++ ["Unprocessed"] ++ (map chShow us) ++ ["end layer"]
    where
      ps = processedClauses l 
      us = unprocessedClauses l
      chShow :: (Clause,History) -> String
      chShow (c,h) = unlines [show c]

  -- TODO
--instance TexShow Layer where

data SymbolType = SymbolPredicate | SymbolFunction | SymbolNilary | SymbolVariable

idtexshow :: ID -> Namespace -> SymbolType -> String
idtexshow id ns tt = case unmap ns id of
  Just x -> x
  Nothing -> case tt of
    SymbolPredicate -> "P_{" ++ show id ++ "}"
    SymbolFunction -> "f_{" ++ show id ++ "}"
    SymbolNilary -> "a_{" ++ show id ++ "}"
    SymbolVariable -> "X_{" ++ show id ++ "}"

instance TexShow Predicate where
  texshow (P True id []) ns = (idtexshow id ns SymbolPredicate)
  texshow (P True id lt) ns = (idtexshow id ns SymbolPredicate) ++ "(" ++ (texshow lt ns) ++ ")"
  texshow (P False id []) ns = "~" ++ (idtexshow id ns SymbolPredicate)
  texshow (P False id lt) ns = "~" ++ (idtexshow id ns SymbolPredicate) ++ "(" ++ (texshow lt ns) ++ ")"

instance TexShow [Predicate] where
  texshow [p] ns = texshow p ns
  texshow (p:l) ns = (texshow p ns) ++ ", " ++ texshow l ns
  texshow [] ns = ""

instance TexShow Term where
  texshow (V id) ns = (idtexshow id ns SymbolVariable)
  texshow (Symbol id []) ns = (idtexshow id ns SymbolNilary)
  texshow (Symbol id tl) ns = (idtexshow id ns SymbolFunction) ++ "(" ++ (texshow tl ns) ++ ")"

instance TexShow [Term] where
  texshow [t] ns = texshow t ns
  texshow (t:l) ns = (texshow t ns) ++ ", " ++ texshow l ns
  texshow [] ns = error "Attempt to texshow an empty list of terms"
