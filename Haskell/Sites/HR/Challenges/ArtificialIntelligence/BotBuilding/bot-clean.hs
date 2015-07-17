{-
0 0
b---d
-d--d
--dd-
--d--
----d


0 1
-b--d
-d--d
--dd-
--d--
----d
-}

{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe

data Move = MoveLeft | MoveRight | MoveDown | MoveUp | Clean
  deriving (Eq, Show)

data Piece = Bot | Dirty | Empty
  deriving (Eq, Show)

data Point = Point Int Int
  deriving (Eq, Show)

data Cell = Cell Piece Point
  deriving (Eq, Show)

data Grid = Grid [Cell]
  deriving (Eq, Show)

data ReducedGrid = ReducedGrid [Cell]
  deriving (Eq, Show)

data Game = Game {
  gameBot  :: !Cell,
  gameGrid :: !ReducedGrid
} deriving (Eq, Show)

-- | createGrid
--
-- >>> createGrid ["b---d"]
-- Grid [Cell Bot (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 1 4),Cell Dirty (Point 1 5)]
createGrid :: [String] -> Grid
createGrid [] = Grid []
createGrid input = Grid $ map (\(p,(x,y)) -> Cell p $ Point x y) $ zip pieces points
  where
    nByN = length $ head input
    points = [ (x, y) | x <- [1..nByN], y <- [1..nByN] ]
    pieces = stringToPieces $ concat input

-- | reduceGrid
--
-- >>> reduceGrid $ createGrid ["b---d", "-d--d", "--dd-", "--d--", "----d"]
-- ReducedGrid [Cell Bot (Point 1 1),Cell Dirty (Point 1 5),Cell Dirty (Point 2 2),Cell Dirty (Point 2 5),Cell Dirty (Point 3 3),Cell Dirty (Point 3 4),Cell Dirty (Point 4 3),Cell Dirty (Point 5 5)]
reduceGrid :: Grid -> ReducedGrid
reduceGrid (Grid cells) = ReducedGrid $ filter (\(Cell p _) -> p /= Empty) cells

-- | createGame
--
-- >>> createGame (1, 1) $ reduceGrid $ createGrid ["b---d"]
-- Game {gameBot = Cell Bot (Point 1 1), gameGrid = ReducedGrid [Cell Dirty (Point 1 5)]}
createGame :: (Int, Int) -> ReducedGrid -> Game
createGame (x, y) (ReducedGrid grid) =
  let
    bot = Cell Bot $ Point x y
  in
  Game {
    gameBot  = maybe (error "No Bot") id (find isBot grid),
    gameGrid = ReducedGrid $ filter (not . isBot) grid
  }

-- | runSingleGame
--
-- >>> runSingleGame $ createGame (1, 1) $ reduceGrid $ createGrid ["b---d", "-d--d", "--dd-", "--d--", "----d"]
runSingleGame :: Game -> Maybe Move
runSingleGame Game{..} =
  if null rgrid
     then Nothing
     else
      let
        sorted_grid = map snd $ sortBy (compare `on` fst) $ map (\cell -> (numberOfMoves gameBot cell, cell)) rgrid
        next_cell = head sorted_grid
        moves = calculateMoves gameBot next_cell
      in
      Just $ head moves
  where
    rgrid = fromReducedGrid gameGrid

-- | runGame
--
-- >>> runGame $ createGame (1, 1) $ reduceGrid $ createGrid ["b---d", "-d--d", "--dd-", "--d--", "----d"]
runGame :: Game -> [Move]
runGame Game{..} =
  if null rgrid
     then []
     else
      let
        sorted_grid = map snd $ sortBy (compare `on` fst) $ map (\cell -> (numberOfMoves gameBot cell, cell)) rgrid
        next_cell = head sorted_grid
        moves = calculateMoves gameBot next_cell
        new_grid = ReducedGrid $ tail sorted_grid
      in
        moves ++ [Clean] ++ (runGame $ Game {
          gameBot = next_cell,
          gameGrid = new_grid
        })
  where
    rgrid = fromReducedGrid gameGrid

-- | calculateMoves
--
-- >>> calculateMoves (Cell Bot (Point 1 2)) (Cell Dirty (Point 1 3))
-- [MoveRight]
--
-- >>> calculateMoves (Cell Bot (Point 1 2)) (Cell Dirty (Point 1 1))
-- [MoveLeft]
--
-- >>> calculateMoves (Cell Bot (Point 1 2)) (Cell Dirty (Point 1 2))
-- []
--
-- >>> calculateMoves (Cell Bot (Point 1 2)) (Cell Dirty (Point 2 2))
-- [MoveDown]
--
-- >>> calculateMoves (Cell Bot (Point 2 2)) (Cell Dirty (Point 1 2))
-- [MoveUp]
--
-- >>> calculateMoves (Cell Bot (Point 2 2)) (Cell Dirty (Point 5 3))
-- [MoveRight,MoveDown,MoveDown,MoveDown]
calculateMoves :: Cell -> Cell -> [Move]
calculateMoves (Cell p1 (Point x1 y1)) (Cell p2 (Point x2 y2))
  | x1 == x2 && y1 == y2 = [Clean]
  | otherwise            = calculateMovesHorizontal (y2 - y1) ++ calculateMovesVertical (x1 - x2)

-- | calculateMoveX
--
-- >>>
calculateMovesHorizontal :: Int -> [Move]
calculateMovesHorizontal 0 = []
calculateMovesHorizontal n = replicate (abs n) move
  where move = if n < 0 then MoveLeft else MoveRight

-- | calculateMoveY
--
-- >>>
calculateMovesVertical :: Int -> [Move]
calculateMovesVertical 0 = []
calculateMovesVertical n = replicate (abs n) move
  where move = if n < 0 then MoveDown else MoveUp

fromReducedGrid :: ReducedGrid -> [Cell]
fromReducedGrid (ReducedGrid grid) = grid

-- | numberOfMoves
--
-- >>> numberOfMoves (Cell Bot $ Point 1 1) (Cell Dirty $ Point 3 1)
-- 2
--
-- numberOfMoves (Cell Bot $ Point 1 1) (Cell Dirty $ Point 3 3)
-- 4
numberOfMoves :: Cell -> Cell -> Int
numberOfMoves (Cell _ (Point x1 y1)) (Cell _ (Point x2 y2)) = abs (x2 - x1) + abs (y2 - y1)

isBot :: Cell -> Bool
isBot (Cell Bot _) = True
isBot _            = False

stringToPieces :: String -> [Piece]
stringToPieces = map charToPiece

charToPiece :: Char -> Piece
charToPiece 'b' = Bot
charToPiece 'd' = Dirty
charToPiece   _ = Empty

prettifyMoves :: [Move] -> String
prettifyMoves moves = unlines $ map showMove moves

showMove :: Move -> String
showMove MoveLeft  = "LEFT"
showMove MoveRight = "RIGHT"
showMove MoveDown  = "DOWN"
showMove MoveUp    = "UP"
showMove Clean     = "CLEAN"

-- original main: plays the whole game. But, challenge only wants one move
main' :: IO ()
main' = do
  (x', y') <- liftM (\line -> (takeWhile (/= ' ') line, tail $ dropWhile (/= ' ') line)) getLine
  let
    (x, y) = (read x' :: Int, read y' :: Int)
  input <- liftM lines getContents
  putStrLn $ prettifyMoves $ runGame $ createGame (x, y) $ reduceGrid $ createGrid input

main :: IO ()
main = do
  (x', y') <- liftM (\line -> (takeWhile (/= ' ') line, tail $ dropWhile (/= ' ') line)) getLine
  let
    (x, y) = (read x' :: Int, read y' :: Int)
  input <- liftM lines getContents
  putStrLn $ showMove $ fromJust $ runSingleGame $ createGame (x+1, (y+1)) $ reduceGrid $ createGrid input
