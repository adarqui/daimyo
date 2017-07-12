module Daimyo.Graph.Array (
  Graph (..),
  mkGraph,
  mkUndirectedAssocs,
  mkEdges,
  adjacent,
  nodes,
  vertices,
  edgeIn,
  inDegree,
  weight,
  edges,
  edgesU,
  sparsity,
  dfsList,
  dfsStackList,
  bfsQueueList,
  tsort
) where

import qualified Daimyo.Queue.List as Queue
import qualified Daimyo.Stack.List as Stack
import           GHC.Arr

-- | Graph
--
newtype Graph a w
  = Graph { runGraph :: Array a [(a, w)] }
  deriving (Show)

-- | Sparsity
--
data Sparsity
  = Sparse Double
  | Dense Double
  deriving (Eq, Show)

-- | mkGraph
--
-- >>> mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double
-- Graph {runGraph = array (1,3) [(1,[(3,56.3),(2,5.5)]),(2,[(1,10.9)]),(3,[])]}
--
mkGraph :: Ix a => (a, a) -> [(a, a, w)] -> Graph a w
mkGraph bounds' es = Graph $ (accumArray accfn [] bounds' es')
  where
    accfn xs x = x:xs
    es' = [ (x1,(x2,w)) | (x1,x2,w) <- es ]

-- | mkUndirectedAssocs
--
mkUndirectedAssocs :: Eq a => [(a, a, t)] -> [(a, (a, t))]
mkUndirectedAssocs es = mkEdges es ++ [ (x2,(x1,w)) | (x1,x2,w) <- es, x1 /= x2 ]

-- | mkEdges
--
mkEdges :: [(a, a, t)] -> [(a, (a, t))]
mkEdges es = [ (x1,(x2,w)) | (x1, x2, w) <- es ]

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

-- | vertices
--
vertices :: Ix a => Graph a w -> [a]
vertices = nodes

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

-- | inDegree
--
-- >>> inDegree 4 (mkGraph (1,4) [(1,2,5.5),(2,1,10.9),(1,3,56.3),(4,3,0.0)] :: Graph Int Double)
-- 0
--
inDegree :: Ix a => a -> Graph a w -> Int
inDegree n (Graph g) = length [ t | v <- vertices (Graph g), t <- adjacent v (Graph g), n == t ]

-- | weight
--
-- >>> weight 1 3 (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- 56.3
--
weight :: Ix a => a -> a -> Graph a w -> w
weight x y (Graph g) = head [ c | (a,c) <- g!x, a == y ]

-- | edges
--
-- >>> edges (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- [(1,3,56.3),(1,2,5.5),(2,1,10.9)]
--
edges :: Ix a => Graph a w -> [(a, a, w)]
edges (Graph g) = [ (v1,v2,w) | v1 <- nodes (Graph g), (v2,w) <- g!v1 ]

-- | edgesU
--
-- kind of broken
--
edgesU :: Ix a => Graph a w -> [(a, a, w)]
edgesU (Graph g) = [ (v1,v2,w) | v1 <- nodes (Graph g), (v2,w) <- g!v1, v1<v2 ]

-- | sparsity
--
-- >>> sparsity (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3)] :: Graph Int Double)
-- Sparse 3.295836866004329
--
-- >>> sparsity (mkGraph (1,3) [(1,2,5.5),(2,1,10.9),(1,3,56.3),(3,1,1.0)] :: Graph Int Double)
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

-- | dfsList
--
-- >>> dfsList 1 (mkGraph (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(6,2,0),(6,5,0),(5,4,0)] :: Graph Int Double)
-- [1,4,3,6,5,2]
--
dfsList :: Ix a => a -> Graph a w -> [a]
dfsList start (Graph g) = reverse $ go [start] []
  where
    go [] visited      = visited
    go (c:cs) visited
      | elem c visited = go cs visited
      | otherwise      = go (adjacent c (Graph g) ++ cs) (c:visited)

-- | dfsStackList
--
-- >>> dfsStackList 1 (mkGraph (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(6,2,0),(6,5,0),(5,4,0)] :: Graph Int Double)
-- [1,4,3,6,5,2]
--
dfsStackList :: Ix a => a -> Graph a w -> [a]
dfsStackList start (Graph g) = reverse $ go (Stack.push start Stack.empty) []
  where
    go s visited
      | Stack.isEmpty s             = visited
      | elem (Stack.top' s) visited = go (Stack.pop' s) visited
      | otherwise                   = go candidates (c:visited)
        where
          c          = Stack.top' s
          candidates = foldr Stack.push (Stack.pop' s) (adjacent c (Graph g))

-- | bfsQueueList
--
-- >>> bfsQueueList 1 (mkGraph (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(6,2,0),(6,5,0),(5,4,0)] :: Graph Int Double)
-- [1,2,3,4,6,5]
--
bfsQueueList :: Ix a => a -> Graph a w -> [a]
bfsQueueList start (Graph g) = reverse $ go (Queue.enqueue start Queue.empty) []
  where
    go q visited
      | Queue.isEmpty q               = visited
      | elem (Queue.front' q) visited = go (Queue.dequeue' q) visited
      | otherwise                     = go candidates (c:visited)
        where
          c          = Queue.front' q
          candidates = foldr Queue.enqueue (Queue.dequeue' q) (adjacent c (Graph g))

-- | tsort
--
-- >>> tsort (mkGraph (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(5,4,0),(6,2,0), (6,5,0)] :: Graph Int Int)
-- [1,3,6,2,5,4]
--
tsort :: Ix a => Graph a w -> [a]
tsort (Graph g) = go [ n | n <- vertices (Graph g), inDegree n (Graph g) == 0 ] []
  where
    go [] r = r
    go (c:cs) visited
      | elem c visited = go cs visited
      | otherwise      = go cs (c:(go (adjacent c (Graph g)) visited))
