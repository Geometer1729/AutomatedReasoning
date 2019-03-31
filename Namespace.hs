module Namespace where

import Data.Map

data Namespace = Namespace {
    forward :: Map String Int,
    reverse :: Map Int String
} deriving (Show)

empty :: Namespace
empty = Namespace Data.Map.empty Data.Map.empty

insert :: Namespace -> String -> (Int, Namespace)
insert ns k = let
    forward' = alter (\x -> case x of
            Just x -> Just x
            Nothing -> Just $ size $ forward ns
        ) k $ forward ns
    ix = forward' ! k
    reverse' = Data.Map.insert ix k $ Namespace.reverse ns
    in (ix, Namespace forward' reverse')

unmap :: Namespace -> Int -> Maybe String
unmap ns k = Data.Map.lookup k $ Namespace.reverse ns
