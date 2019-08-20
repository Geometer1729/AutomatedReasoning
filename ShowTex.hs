{-# LANGUAGE FlexibleInstances #-}

module ShowTex where
import Types
import Namespace
import Data.List

class NsShow a where
  nsshow :: a -> Namespace -> String

class TexShow a where
  texshow :: a -> Namespace -> String

idshow :: ID -> Namespace -> SymbolType -> String
idshow i ns tt = case unmap ns i of
  Just x -> x
  Nothing -> case tt of
    SymbolPredicate -> "P" ++ show i
    SymbolFunction -> "f" ++ show i
    SymbolNilary -> "a" ++ show i
    SymbolVariable -> "X" ++ show i

instance NsShow Predicate where
  nsshow (P True i []) ns = idshow i ns SymbolPredicate
  nsshow (P True i lt) ns = (idshow i ns SymbolPredicate) ++ "(" ++ (nsshow lt ns) ++ ")"
  nsshow (P False i []) ns = "~" ++ idshow i ns SymbolPredicate
  nsshow (P False i lt) ns = "~" ++ (idshow i ns SymbolPredicate) ++ "(" ++ (nsshow lt ns) ++ ")"

instance {-# OVERLAPPING #-} NsShow [Predicate] where
  nsshow [p] ns = (nsshow p ns)
  nsshow (p:l) ns = (nsshow p ns) ++ ", " ++ nsshow l ns
  nsshow [] _ = ""

instance NsShow Term where
  nsshow (V i) ns = idshow i ns SymbolVariable
  nsshow (Symbol i []) ns = idshow i ns SymbolNilary
  nsshow (Symbol i tl) ns = (idshow i ns SymbolFunction) ++ "(" ++ (nsshow tl ns) ++ ")"

instance {-# OVERLAPPING #-} NsShow [Term] where
  nsshow [t] ns = nsshow t ns
  nsshow (t:l) ns = (nsshow t ns) ++ ", " ++ nsshow l ns
  nsshow [] _ = error "Attempt to nsshow an empty list of terms"

instance NsShow Layer where
  nsshow l ns = unlines $ "begin layer\nProcesed" : (map chShow ps) ++ ["Unprocessed"] ++ (map chShow us) ++ ["end layer","imps:", imps]
    where
      ps = processedClauses l 
      us = unprocessedClauses l
      chShow :: (Clause,History) -> String
      chShow (c,_) = unlines [nsshow c ns]
      imps = unlines [ intercalate "," [ nsshow h ns | h <- hs ] ++ "->" ++ nsshow c ns ++ "\n" | (hs,c) <- (processedImplications l ++ unprocessedImplications l) ]


  -- TODO
--instance TexShow Layer where

data SymbolType = SymbolPredicate | SymbolFunction | SymbolNilary | SymbolVariable

idtexshow :: ID -> Namespace -> SymbolType -> String
idtexshow i ns tt = case unmap ns i of
  Just x -> x
  Nothing -> case tt of
    SymbolPredicate -> "P_{" ++ show i ++ "}"
    SymbolFunction -> "f_{" ++ show i ++ "}"
    SymbolNilary -> "a_{" ++ show i ++ "}"
    SymbolVariable -> "X_{" ++ show i ++ "}"

instance TexShow Predicate where
  texshow (P True i []) ns = (idtexshow i ns SymbolPredicate)
  texshow (P True i lt) ns = (idtexshow i ns SymbolPredicate) ++ "(" ++ (texshow lt ns) ++ ")"
  texshow (P False i []) ns = "~" ++ (idtexshow i ns SymbolPredicate)
  texshow (P False i lt) ns = "~" ++ (idtexshow i ns SymbolPredicate) ++ "(" ++ (texshow lt ns) ++ ")"

instance TexShow [Predicate] where
  texshow [p] ns = texshow p ns
  texshow (p:l) ns = (texshow p ns) ++ ", " ++ texshow l ns
  texshow [] _ = ""

instance TexShow Term where
  texshow (V i) ns = (idtexshow i ns SymbolVariable)
  texshow (Symbol i []) ns = (idtexshow i ns SymbolNilary)
  texshow (Symbol i tl) ns = (idtexshow i ns SymbolFunction) ++ "(" ++ (texshow tl ns) ++ ")"

instance TexShow [Term] where
  texshow [t] ns = texshow t ns
  texshow (t:l) ns = (texshow t ns) ++ ", " ++ texshow l ns
  texshow [] _ = error "Attempt to texshow an empty list of terms"
