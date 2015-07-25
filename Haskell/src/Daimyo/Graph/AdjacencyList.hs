module Daimyo.Graph.AdjacencyList (
  Graph,
) where

-- | Graph
--
-- n: node type
-- w: weight type
--
type Graph n w = [(n, [(n,w)])]
