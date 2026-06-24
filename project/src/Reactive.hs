module Reactive
  ( updateCell
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
import Graph (cellDeps, findCyclesAndOrder)
import Eval (evalExpr)

updateCell :: Addr -> Content -> Sheet -> Env -> (Sheet, Env)
updateCell targetAddr newContent sheet oldEnv = (newSheet, finalEnv)
  where
    newCells = Map.insert targetAddr (Cell targetAddr newContent) (unSheet sheet)
    newSheet = Sheet newCells

    depsMap = Map.map cellDeps newCells
    dependentsMap = Map.fromListWith (++)
      [ (dep, [addr'])
      | (addr', depsList) <- Map.toList depsMap
      , dep <- depsList
      ]

    affected = dfs targetAddr
    dfs start = go [start] []
      where
        go [] visited = visited
        go (x:xs) visited
          | x `elem` visited = go xs visited
          | otherwise =
              let nexts = Map.findWithDefault [] x dependentsMap
              in go (nexts ++ xs) (x : visited)

    affectedSet = Set.fromList affected

    (newAcyclicOrder, newCycles) = findCyclesAndOrder newSheet

    allNewCycleAddrs = concat newCycles
    envWithNewCycles = foldr (\a acc -> Map.insert a (ErrV "cycle") acc) oldEnv allNewCycleAddrs

    affectedAcyclic = filter (`Set.member` affectedSet) newAcyclicOrder

    finalEnv = foldl evalCell envWithNewCycles affectedAcyclic

    evalCell :: Env -> Addr -> Env
    evalCell currentEnv addr' =
      case Map.lookup addr' newCells of
        Nothing -> currentEnv
        Just cell ->
          let val = case content cell of
                      Lit v -> v
                      Form expr -> evalExpr currentEnv expr
          in Map.insert addr' val currentEnv
