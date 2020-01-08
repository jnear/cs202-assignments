{-# LANGUAGE Strict #-}
module Graph where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Graph a = Map a (Set a)

emptyGraph :: Graph a
emptyGraph = Map.empty

addEdge :: (Ord a) => Graph a -> a -> a -> Graph a
addEdge g from to | from == to = g
                  | otherwise =
  let g1 = Map.insertWith Set.union from (Set.singleton to)   g
      g2 = Map.insertWith Set.union to   (Set.singleton from) g1
  in g2

neighbors :: (Ord a) => Graph a -> a -> Set a
neighbors g v | Map.member v g = g Map.! v
              | otherwise      = Set.empty

vertices :: Graph a -> Set a
vertices = Map.keysSet

showGraph :: (Show a, Ord a) => Graph a -> String
showGraph g = concat $ map (\x -> (show x) ++ " -> " ++ (show $ g Map.! x) ++ "\n") (Set.toList $ vertices g)
