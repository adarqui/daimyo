module Daimyo.Graph.AdjacencyMatrix (
  Graph (..),
  Sparsity (..),
  mkGraph,
  mkUndirectedAssocs,
  adjacent,
  nodes,
  vertices,
  weight,
  edgeIn,
  edges,
  sparsity
) where

import           Data.Maybe
import           GHC.Arr

-- | Graph
--
newtype Graph a w
  = Graph { runGraph :: Array (a,a) (Maybe w) }
  deriving (Show)

-- | Sparsity
--
data Sparsity
  = Sparse Double
  | Dense Double
  deriving (Eq, Show)

-- | mkGraph
--
-- >>> mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double
-- Graph {runGraph = array ((1,1),(3,3)) [((1,1),Nothing),((1,2),Just 5.5),((1,3),Just 56.3),((2,1),Just 10.9),((2,2),Nothing),((2,3),Nothing),((3,1),Nothing),((3,2),Nothing),((3,3),Nothing)]}
--
mkGraph :: Ix a => (a, a) -> [((a, a),w)] -> Graph a w
mkGraph b@(l,u) es = Graph $ emptyArray // es'
  where
    emptyArray = array ((l,l),(u,u)) [ ((x1,x2),Nothing) | x1 <- range b, x2 <- range b]
    es' = [ ((x1, x2),Just w) | ((x1,x2),w) <- es ]

-- | mkUndirectedAssocs
--
mkUndirectedAssocs :: Eq a => [((a, a), t)] -> [((a, a), t)]
mkUndirectedAssocs es = es ++ [ ((x2,x1),w) | ((x1,x2),w) <- es, x1 /= x2 ]

-- | adjacent
--
-- >>> adjacent 1 (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- [2,3]
--
adjacent :: (Ix a, Eq w) => a -> Graph a w -> [a]
adjacent v1 (Graph g) = [ v2 | v2 <- nodes (Graph g), (g!(v1,v2))/=Nothing ]

-- | nodes
--
-- >>> nodes (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- [1,2,3]
--
nodes :: Ix a => Graph a w -> [a]
nodes (Graph g) = range (l,u)
  where
    ((l,_),(u,_)) = bounds g

-- | vertices
--
vertices :: Ix a => Graph a w -> [a]
vertices = nodes

-- | edgeIn
--
-- >>> edgeIn (1,2) (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- True
--
-- >>> edgeIn (2,2) (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- False
--
edgeIn :: (Ix a, Eq w) => (a, a) -> Graph a w -> Bool
edgeIn (x,y) (Graph g) = g ! (x,y) /= Nothing

-- | weight
--
-- >>> weight 1 3 (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- 56.3
--
weight :: Ix a => a -> a -> Graph a w -> w
weight x y (Graph g) = fromJust (g ! (x,y))

-- | edges
--
-- >>> edges (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- [((1,2),5.5),((1,3),56.3),((2,1),10.9)]
--
edges :: (Ix a, Eq w) => Graph a w -> [((a, a), w)]
edges (Graph g) = [ ((v1,v2),fromJust (g!(v1,v2))) | v1 <- nodes (Graph g), v2 <- nodes (Graph g), edgeIn (v1, v2) (Graph g) ]

-- | sparsity
--
-- >>> sparsity (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3)] :: Graph Int Double)
-- Sparse 3.295836866004329
--
-- >>> parsity (mkGraph (1,3) [((1,2),5.5),((2,1),10.9),((1,3),56.3),((3,1),1.0)] :: Graph Int Double)
-- Dense 3.295836866004329
--
sparsity :: (Ix a, Eq w) => Graph a w -> Sparsity
sparsity g
  | e < z      = Sparse z
  | otherwise  = Dense z
  where
    e = fromIntegral $ length $ edges g
    v = fromIntegral $ length $ vertices g
    z = v*log v
