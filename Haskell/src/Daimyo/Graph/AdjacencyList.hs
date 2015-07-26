module Daimyo.Graph.AdjacencyList (
  Graph,
  mkGraph {-,
  mkUndirectedAssocs,
  mkEdges,
  adjacent,
  nodes,
  vertices,
  edgeIn,
  weight,
  edges,
  edgesU,
  sparsity,
  dfsList,
  dfsStackList,
  bfsList,
  bfsQueueList
  -}
) where

import qualified Daimyo.Tree.AVL as Tree
import qualified Daimyo.Queue.List as Queue
import qualified Daimyo.Stack.List as Stack

-- | GraphInternal
--
data GraphInternal a w = GI {
  bounds :: (Int, Int),
  tree :: Tree.AVLTree (a, [(a, w)])
}

-- | Graph
--
newtype Graph a w
  = Graph { runGraph :: Tree.AVLTree (a, [(a, w)]) }
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
mkGraph :: (a, a) -> [(a, a, w)] -> Graph a w
mkGraph = undefined
{-
mkGraph bounds' es = Graph $ (accumArray accfn [] bounds' es')
  where
    accfn xs x = x:xs
    es' = [ (x1,(x2,w)) | (x1,x2,w) <- es ]
    -}
