module Daimyo.Game.TileN (
  Board,
  Position,
  manDist,
  isAdjacent,
  allMoves,
  g4T,
  s4T,
  g8T,
  s8T,
  goal,
  succs,
  dfsNT,
  dfs4T,
  dfs8T
) where

import           Daimyo.Algorithm.TopDown.BacktrackSearch
import           GHC.Arr

type Position = (Int, Int)
type Board = Array Int Position

data Boards
  = Boards [Board]
  deriving (Eq, Show)

-- | manDist
--
-- >>> manDist (3,1) (2,3)
-- 3
--
-- >>> manDist (1,2) (1,1)
-- 1
--
manDist :: Position -> Position -> Int
manDist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- | isAdjacent
--
-- >>> isAdjacent (3,1) (2,3)
-- False
--
-- >>> isAdjacent (1,2) (1,1)
-- True
--
isAdjacent :: Position -> Position -> Bool
isAdjacent p1 p2 = manDist p1 p2 == 1

-- | allMoves
--
allMoves :: Int -> Board -> [Board]
allMoves sz b = [ b // [(0,b!i),(i,b!0) ] | i <- range (0,sz), isAdjacent (b!0) (b!i) ]

-- | succs
--
succs :: Int -> Boards -> [Boards]
succs _ (Boards [])          = []
succs sz (Boards (n@(b:bs))) = filter (notIn bs) [ Boards (b':n) | b' <- allMoves sz b ]
    where
      notIn _ (Boards [])     =  True
      notIn xs (Boards (x:_)) = not (elem (elems x) (map elems xs))

-- | goal8T
--
goal :: Board -> Boards -> Bool
goal _ (Boards [])        = False
goal board (Boards (n:_)) = elems n == elems board

-- | dfs8T
--
dfsNT :: Board -> Board -> (Board -> Boards -> Bool) -> (Boards -> [Boards]) -> [[Position]]
dfsNT start_board goal_board goalFn succsFn = map elems ls
  where
    ((Boards ls):_) = searchDFS succsFn (goalFn goal_board) (Boards [start_board])

-- | g8T
--
g8T :: Board
g8T = array (0,8) board
  where
    board = [(0,(2,2)),(1,(1,1)),(2,(1,2)),
             (3,(1,3)),(4,(2,3)),(5,(3,3)),
             (6,(3,2)),(7,(3,1)),(8,(2,1))]

-- | s8T
--
s8T :: Board
s8T = array (0,8) board
  where
    board = [(0,(2,2)),(1,(1,2)),(2,(1,1)),
             (3,(3,3)),(4,(2,1)),(5,(3,2)),
             (6,(1,3)),(7,(3,1)),(8,(2,3))]

-- | g4T
--
g4T :: Board
g4T = array (0,4) board
  where
    board = [(0,(2,2)),(1,(1,1)),
             (2,(1,2)),(3,(2,1))]

-- | s4T
--
s4T :: Board
s4T = array (0,4) board
  where
    board = [(0,(2,2)),(1,(1,2)),
             (2,(1,1)),(3,(2,1))]

dfs4T :: [[Position]]
dfs4T = dfsNT s4T g4T goal (succs 4)

dfs8T :: [[Position]]
dfs8T = dfsNT s8T g8T goal (succs 8)
