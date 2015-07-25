module Daimyo.Graph.Array (
  Graph (..),
  mkGraph,
  mkUndirectedAssocs,
  mkEdges,
  adjacent,
  nodes,
  edgeIn,
  weight,
  edgesD,
  edgesU
) where

import GHC.Arr

-- | Graph
--
newtype Graph a w
  = Graph { runGraph :: Array a [(a, w)] }
  deriving (Show)

-- | mkGraph
--
-- >>> mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double
-- Graph {runGraph = array (1,3) [(1,[(3,56.3),(2,5.5)]),(2,[(1,10.9)]),(3,[])]}
--
mkGraph :: Ix a => (a, a) -> [(a, a, w)] -> Graph a w
mkGraph bounds' edges = Graph $ (accumArray accfn [] bounds' edges')
  where
    accfn xs x = x:xs
    edges' = [ (x1,(x2,w)) | (x1,x2,w) <- edges ]

-- | mkUndirectedAssocs
--
mkUndirectedAssocs :: Eq a => [(a, a, t)] -> [(a, (a, t))]
mkUndirectedAssocs edges = mkEdges edges ++ [ (x2,(x1,w)) | (x1,x2,w) <- edges, x1 /= x2 ]

-- | mkEdges
--
mkEdges :: [(a, a, t)] -> [(a, (a, t))]
mkEdges edges = [ (x1,(x2,w)) | (x1, x2, w) <- edges ]

-- | adjacent
--
-- >>> adjacent 1 (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- [3,2]
--
adjacent :: Ix a => a -> Graph a w -> [a]
adjacent v (Graph g) = map fst (g!v)

-- | nodes
--
-- >>> nodes (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- [1,2,3]
--
nodes :: Ix a => Graph a w -> [a]
nodes (Graph g) = indices g

-- | edgeIn
--
-- >>> edgeIn (1,2) (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- True
--
-- >>> edgeIn (1,4) (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- False
--
edgeIn :: Ix a => (a, a) -> Graph a w -> Bool
edgeIn (x,y) (Graph g) = elem y (adjacent x (Graph g))

-- | weight
--
-- >>> weight 1 3 (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- 56.3
--
weight :: Ix a => a -> a -> Graph a w -> w
weight x y (Graph g) = head [ c | (a,c) <- g!x, a == y ]

-- | edgesD
--
-- >>> edgesD (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- [(1,3,56.3),(1,2,5.5),(2,1,10.9)]
--
edgesD :: Ix a => Graph a w -> [(a, a, w)]
edgesD (Graph g) = [ (v1,v2,w) | v1 <- nodes (Graph g), (v2,w) <- g!v1 ]

-- | edgesU
--
-- kind of broken
--
edgesU :: Ix a => Graph a w -> [(a, a, w)]
edgesU (Graph g) = [ (v1,v2,w) | v1 <- nodes (Graph g), (v2,w) <- g!v1, v1<v2 ]
