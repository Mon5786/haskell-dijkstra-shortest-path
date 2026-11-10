module Main where

import System.Environment (getArgs)
import Route
import qualified Graph as G
import Data.Int (Int32)

type Name  = String
type Cost  = Int
type Graph = G.Graph Name

-- Use the shortestPath function from Graph.
shortestPath :: Graph -> Name -> Name -> Maybe ([Name], Cost)
shortestPath = G.shortestPath

-- Use a large value when no path is found.
noPath :: Cost
noPath = fromIntegral (maxBound :: Int32)

-- Create a directed graph using the stops and lines.
buildGraph :: [Stop] -> [LineTable] -> Graph
buildGraph stops lineTables =
  let nodes = map name stops
      edges = concatMap lineToEdges lineTables
  in G.fromEdges edges (G.fromNodes nodes)

-- The travel time between two stops is used as the edge cost.
lineToEdges :: LineTable -> [(Name, Name, Cost)]
lineToEdges (LineTable _ stops) = go stops
  where
    go (LineStop curr _ : rest@(LineStop next time : _)) =
      (curr, next, fromIntegral time) : go rest
    go _ = []

printPath :: ([Name], Cost) -> IO ()
printPath (path, cost) = do
  print cost
  mapM_ putStrLn path

main :: IO ()
main = do
  args <- getArgs
  case args of
    [stopFile, lineFile, src, dst] -> do
      stopsList <- readStops stopFile
      linesList <- readLines lineFile
      case (stopsList, linesList) of
        (Right stops, Right lineTables) -> do
          let graph = buildGraph stops lineTables
          case shortestPath graph src dst of
            Just result -> printPath result
            Nothing     -> print noPath
        _ -> print noPath
    _ -> print noPath
