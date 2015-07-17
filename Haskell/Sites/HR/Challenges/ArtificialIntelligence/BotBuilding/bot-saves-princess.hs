{-# OPTIONS_GHC -O2 #-}

{-
-
- this code is weird.... don't read much into it
-

3
---
-m-
p--
Sample output

DOWN
LEFT
-}

module Main where

import           Data.Maybe
import           Prelude       hiding (init, lookup, map, zip, zipWith)
import           System.Random

--
-- Prelude stuff
--

square :: Num a => a -> a
square n = n * n

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM f act = do
  v <- act
  return $ f v

-- | listComp1
--
-- f = function to apply
-- xs = list
-- p = predicate
--
-- >>> listComp1 (+1) (const True) [1..10]
-- [2,3,4,5,6,7,8,9,10,11]
--
-- >>> listComp1 id (\x -> x >= 5) [1..10]
-- [5,6,7,8,9,10]
listComp1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp1 _ p [] = []
listComp1 f p (x:xs)
  | p x == True = f x : listComp1 f p xs
  | otherwise   = listComp1 f p xs

--- | listComp2
--
-- >>> listComp2 (\x y -> (x,y)) (const $ const True) [1..3] [1..3]
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
listComp2 :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
listComp2 _ _ [] _ = []
listComp2 f p (x:xs) ys = listComp1 (f x) (p x) ys ++ listComp2 f p xs ys

-- | lookup
--
-- >>>
lookup :: (a -> Bool) -> [a] -> Maybe a
lookup _ [] = Nothing
lookup test (x:xs) =
  if test x
     then Just x
     else lookup test xs

-- | replace
--
-- >>> replace (5, (1, 0)) (5, (0, 1)) [(1, (0, 0)), (2, (0, 0)), (3, (0, 0)), (5, (1, 0))]
-- [(1,(0,0)),(2,(0,0)),(3,(0,0)),(5,(0,1))]
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ []         = []
replace from to (x:xs)
  | from == x = to : xs
  | otherwise = x : replace from to xs

-- | init
--
-- >>> init [1..3]
-- [1,2]
init :: [a] -> [a]
init []         = []
init (x:[])     = []
init (x:xs) = x : init xs

--
-- Challenge
--

data Game = Game {
  gameSeed   :: !Int,
  gameStates :: ![(Move, Grid)]
} deriving (Show)

data Move = MoveLeft | MoveRight | MoveUp | MoveDown | MoveNone
  deriving (Show)

data Piece = Princess | Bot | Empty
  deriving (Show)

data Point = Point Int Int
  deriving (Show)

data Cell = Cell Piece Point
  deriving (Show)

type Grid = [Cell]

instance Eq Move where
  MoveLeft == MoveLeft   = True
  MoveRight == MoveRight = True
  MoveDown == MoveDown   = True
  MoveUp == MoveUp       = True
  MoveNone == MoveNone   = True
  _ == _                 = False

instance Eq Piece where
  Princess == Princess = True
  Bot == Bot           = True
  Empty == Empty       = True
  _ == _               = False

instance Eq Point where
  Point x1 y1 == Point x2 y2 = x1 == x2 && y1 == y2

instance Eq Cell where
  Cell piece1 point1 == Cell piece2 point2 = piece1 == piece2 && point1 == point2

instance Random Move where
  random g =
    let
      (v, g') = next g
    in
      (int2Move (v `mod` 4), g')

int2Move :: Int -> Move
int2Move 0 = MoveLeft
int2Move 1 = MoveRight
int2Move 2 = MoveDown
int2Move 3 = MoveUp

showMove :: Move -> String
showMove MoveLeft  = "LEFT"
showMove MoveRight = "RIGHT"
showMove MoveDown  = "DOWN"
showMove MoveUp    = "UP"
showMove MoveNone  = ""

-- | randomElem
--
-- >>> fst $ fromJust $ randomElem (mkStdGen 5) [1..10]
-- 4
--
-- >>> fst $ fromJust $ randomElem (mkStdGen 100) [1..10]
-- 8
randomElem :: StdGen -> [a] -> Maybe (a, StdGen)
randomElem _ [] = Nothing
randomElem g xs =
  let
    (v, g') = random g :: (Int, StdGen)
  in
    Just (xs !! (v `mod` length xs), g')

-- | Create empty grid
--
-- >>> emptyGrid 3
-- [Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 1),Cell Empty (Point 2 2),Cell Empty (Point 2 3),Cell Empty (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)]
emptyGrid :: Int -> Grid
emptyGrid n = map (\(x, y) -> Cell Empty $ Point x y) coords
  where
    coords = listComp2 (\a b -> (a, b)) (\x y -> True) [1..n] [1..n]

-- | Overlay grid with input
--
-- >>> overlayGrid (emptyGrid 3) "----m-p--"
-- [Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 1),Cell Bot (Point 2 2),Cell Empty (Point 2 3),Cell Princess (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)]
overlayGrid :: Grid -> String -> Grid
overlayGrid g s = zipWith (\(Cell piece point) y -> Cell (translateInput y) point) g s

-- | Translate an input character to a Piece
--
-- >>> findElem Bot $ overlayGrid (emptyGrid 3) "----m-p--"
-- Just (Cell Bot (Point 2 2))
--
-- >>> findElem Princess $ overlayGrid (emptyGrid 3) "----m-p--"
-- Just (Cell Princess (Point 3 1))
translateInput :: Char -> Piece
translateInput 'p' = Princess
translateInput 'm' = Bot
translateInput  _  = Empty

-- | findElem: find the first occurrence of the elemt in the grid
--
-- >>> findElem Princess $ overlayGrid (emptyGrid 3) "----m-p--"
-- Just (Cell Princess (Point 2 2))
--
-- >>> findElem Bot $ overlayGrid (emptyGrid 3) "----m-p--"
-- Just (Cell Bot (Point 3 1))
findElem :: Piece -> Grid -> Maybe Cell
findElem piece = lookup (\(Cell piece' point') -> piece' == piece)

-- | calculateMoves
--
-- >>> calculateMoves (Point 2 2) (Point 3 1)
-- Just [MoveLeft,MoveDown]
--
-- >>> calculateMoves (Point 3 2) (Point 3 1)
-- Just [MoveLeft]
--
-- >>> calculateMoves (Point 3 1) (Point 3 1)
-- Nothing
calculateMoves :: Point -> Point -> Maybe [Move]
calculateMoves (Point fromX fromY) (Point toX toY) =
  if [] == moves
     then Just $ [MoveNone]
     else Just $ moves
  where
    left  = if fromY > toY then [MoveLeft] else []
    right = if fromY < toY then [MoveRight] else []
    down  = if fromX < toX then [MoveDown] else []
    up    = if fromX > toX then [MoveUp] else []
    moves = concat [left, right, down, up]

-- | calculateMove
--
-- I should be passing a grid with bounds etc to make sure you can't go too far in any direction but i'm just bored right now. ;f
--
-- >>> calculateMove MoveLeft (Point 3 2)
-- Just (Point 3 1)
--
-- >>> calculateMove MoveLeft (Point 3 0)
-- Nothing
calculateMove :: Move -> Point -> Maybe Point
calculateMove MoveLeft (Point x y)
  | y - 1 < 1 = Nothing
  | otherwise = Just $ Point x (y-1)
calculateMove MoveRight (Point x y) = Just $ Point x (y+1)
calculateMove MoveDown (Point x y) = Just $ Point (x+1) y
calculateMove MoveUp (Point x y)
  | x - 1 < 1 = Nothing
  | otherwise = Just $ Point x (y+1)
calculateMove MoveNone _ = Nothing

-- | swapCells
--
-- I should be using an array/matrix yada yada
-- >>> swapCells (Cell Princess (Point 2 2)) (Cell Bot (Point 3 1)) $ overlayGrid (emptyGrid 3) "----m-p--"
-- Just [Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 1),Cell Bot (Point 2 2),Cell Empty (Point 2 3),Cell Princess (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)]
swapCells :: Cell -> Cell -> Grid -> Maybe Grid
swapCells c1@(Cell piece1 point1) c2@(Cell piece2 point2) grid = Just $ replace c1 (Cell piece1 point2) $ replace c2 (Cell piece2 point1) grid

gameMoves :: Game -> [Move]
gameMoves game = map fst $ gameStates game

gameOutput4HackerRank :: [Move] -> String
gameOutput4HackerRank [] = ""
gameOutput4HackerRank (x:xs) = showMove x ++ "\n" ++ gameOutput4HackerRank xs

--  | findPrincess
--
-- >>> findPrincess 3 $ overlayGrid (emptyGrid 3) "----m-p--"
-- Game {gameSeed = 3, gameStates = [(MoveLeft,[Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 1),Cell Bot (Point 2 2),Cell Empty (Point 2 3),Cell Princess (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)]),(MoveDown,[Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 2),Cell Bot (Point 2 1),Cell Empty (Point 2 3),Cell Princess (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)]),(MoveNone,[Cell Empty (Point 1 1),Cell Empty (Point 1 2),Cell Empty (Point 1 3),Cell Empty (Point 2 2),Cell Bot (Point 3 1),Cell Empty (Point 2 3),Cell Princess (Point 3 1),Cell Empty (Point 3 2),Cell Empty (Point 3 3)])]}
--
findPrincess :: Int -> Grid -> Game
findPrincess seed grid =
  Game {
    gameSeed = seed,
    gameStates = maybe [] id $ findPrincess' (mkStdGen seed) grid
  }

findPrincess' :: StdGen -> Grid -> Maybe [(Move, Grid)]
findPrincess' g grid = do
  (Cell princess princess_position) <- findElem Princess grid
  (Cell bot bot_position) <- findElem Bot grid
  moves <- calculateMoves bot_position princess_position
  (move, g') <- randomElem g moves
  case move of
    MoveNone -> return [(MoveNone, grid)] -- Done!
    otherwise -> do
      new_position <- calculateMove move bot_position
      grid' <- swapCells (Cell bot bot_position) (Cell Empty new_position) grid
      vs <- findPrincess' g' grid'
      return $ (move, grid) : vs

main' :: IO ()
main' = do
  rows_cols <- readLn :: IO Int
  input <- liftM (filter (/= '\n')) getContents
  random_seed <- randomIO :: IO Int
  putStr $ gameOutput4HackerRank $ init $ gameMoves $ findPrincess random_seed $ overlayGrid (emptyGrid rows_cols) input

--
-- didn't realize they had their own main
--

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)


getPoint :: Cell -> Point
getPoint (Cell _ point) = point

--
-- simple solution just to get some points
-- ick
--

displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess rows_cols input =
  let
    grid = overlayGrid (emptyGrid rows_cols) $ concat input
    (Just princess) = findElem Princess grid
    (Just bot) = findElem Bot grid
  in
    gameOutput4HackerRank $ displayPathtoPrincess'' bot princess

displayPathtoPrincess'' bot princess =
  let
    (Just moves) = calculateMoves (getPoint bot) (getPoint princess)
    new_bot_point = calculateMove (head moves) (getPoint bot)
  in
    case new_bot_point of
      Nothing -> []
      _  -> head moves : displayPathtoPrincess'' (Cell Bot (fromJust new_bot_point)) princess


--    gameOutput4HackerRank $ init $ gameMoves $ findPrincess rows_cols $ overlayGrid (emptyGrid rows_cols) $ concat input

displayPathtoPrincess' :: Int -> [String] -> String
displayPathtoPrincess' rows_cols input =
    gameOutput4HackerRank $ init $ gameMoves $ findPrincess rows_cols $ overlayGrid (emptyGrid rows_cols) $ concat input

main :: IO ()
main = do
    n <- getLine
    let i = read n
    grid <- getList i
    putStrLn.displayPathtoPrincess i $ grid
