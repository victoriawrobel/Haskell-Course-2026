module Graph
  ( colToIndex
  , indexToCol
  , expandRange
  , deps
  , cellDeps
  , findCyclesAndOrder
  ) where

import Data.List (nub)
import Data.Graph (stronglyConnComp, SCC(..))
import qualified Data.Map as Map
import Types

colToIndex :: String -> Int
colToIndex = foldl (\acc c -> acc * 26 + (fromEnum c - fromEnum 'A' + 1)) 0

indexToCol :: Int -> String
indexToCol n
  | n <= 0    = ""
  | otherwise = indexToCol q ++ [d]
  where
    (q, r) = (n - 1) `divMod` 26
    d = toEnum (fromEnum 'A' + r)

expandRange :: Addr -> Addr -> [Addr]
expandRange (c1, r1) (c2, r2) =
  [ (indexToCol c, r)
  | c <- [min col1 col2 .. max col1 col2]
  , r <- [min r1 r2 .. max r1 r2]
  ]
  where
    col1 = colToIndex c1
    col2 = colToIndex c2

deps :: Expr -> [Addr]
deps expr = nub (go expr)
  where
    go (Ref addr') = [addr']
    go (LitE _) = []
    go (BinOp _ e1 e2) = go e1 ++ go e2
    go (RangeOp _ start end) = expandRange start end

cellDeps :: Cell -> [Addr]
cellDeps cell = case content cell of
  Lit _ -> []
  Form expr -> deps expr

findCyclesAndOrder :: Sheet -> ([Addr], [[Addr]])
findCyclesAndOrder sheet = (acyclicOrder, cycles)
  where
    cellsList = Map.elems (unSheet sheet)
    triples = [ (cell, addr cell, cellDeps cell) | cell <- cellsList ]
    sccs = stronglyConnComp triples

    acyclicOrder = [ addr cell | AcyclicSCC cell <- sccs ]
    cycles = [ map addr cells | CyclicSCC cells <- sccs ]
