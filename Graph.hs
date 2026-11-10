module Graph
  ( Graph
  , Cost
  , empty
  , addNode
  , addEdge
  , fromNodes
  , fromEdges
  , shortestPath
  ) where

import qualified Data.Map.Strict as M
import qualified Data.PSQueue    as PQ
import Data.PSQueue (Binding((:->)))
import Data.List   (foldl')

type Cost = Int

-- Directed graph represented by adjacency lists.
newtype Graph a = Graph (M.Map a [(a, Cost)])
  deriving (Show)

empty :: Graph a
empty = Graph M.empty

addNode :: Ord a => a -> Graph a -> Graph a
addNode x (Graph g)
  | M.member x g = Graph g
  | otherwise    = Graph (M.insert x [] g)

-- Edges are directed; each edge stores its travel cost.
addEdge :: Ord a => a -> a -> Cost -> Graph a -> Graph a
addEdge src dst cost (Graph g) =
  Graph (M.insertWith (++) src [(dst, cost)] g)

fromNodes :: Ord a => [a] -> Graph a
fromNodes = foldl' (flip addNode) empty

fromEdges :: Ord a => [(a, a, Cost)] -> Graph a -> Graph a
fromEdges edges g = foldl' addOne g edges
  where
    addOne acc (src, dst, cost) = addEdge src dst cost acc

neighbours :: Ord a => Graph a -> a -> [(a, Cost)]
neighbours (Graph g) x =
  case M.lookup x g of
    Just ns -> ns
    Nothing -> []

member :: Ord a => a -> Graph a -> Bool
member x (Graph g) =
  case M.lookup x g of
    Just _ -> True
    Nothing -> False

-- Dijkstra's algorithm: expand the lowest-cost unvisited node first.
-- Return Nothing when the destination cannot be reached.
shortestPath :: Ord a => Graph a -> a -> a -> Maybe ([a], Cost)
shortestPath g src dst
  | not (member src g) = Nothing
  | not (member dst g) = Nothing
  | src == dst = Just ([src], 0)
  | otherwise = dijkstra (PQ.singleton src (0, [src])) M.empty
  where
    dijkstra queue visited =
      case PQ.minView queue of
        Nothing -> Nothing
        Just (node :-> (cost, path), queue')
          | M.member node visited -> dijkstra queue' visited
          | node == dst -> Just (reverse path, cost)
          | otherwise ->
              let visited' = M.insert node cost visited
                  queue'' = foldr (addNeighbour cost path visited') queue' (neighbours g node)
              in dijkstra queue'' visited'

    addNeighbour cost path visited' (nbr, w) queue
      | M.member nbr visited' = queue
      | otherwise =
          let newCost = cost + w
              newPath = nbr : path
          in case PQ.lookup nbr queue of
               Just (oldCost, _) | oldCost <= newCost -> queue
               _ -> PQ.insert nbr (newCost, newPath) queue