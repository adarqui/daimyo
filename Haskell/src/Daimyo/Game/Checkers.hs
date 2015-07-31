module Daimyo.Game.Checkers (
  Piece (..),
  Square (..),
  Board (..),
  newBoard,
  defaultBoard,
  colorCycle
) where

import           Data.List
import           Data.Matrix

-- source: http://simple.wikipedia.org/wiki/Checkers

--
--        a      b      c      d      e      f      g      h
--    8 white, black, white, black, white, black, white, black
--    7 black, white, black, white, black, white, black, white
--    ...
--    2 white, black, white, black, white, black, white, black
--    1 black, white, black, white, black, white, black, white
--        a      b      c      d      e      f      g      h
--

data Piece
  = Light
  | Dark
  deriving (Show, Eq)

data Square
  = White
  | Black
  deriving (Show, Eq, Enum)

data Board = Board {
  board :: Matrix (Square, Maybe Piece)
} deriving (Show)

-- | newBoard
--
--newBoard
--Board {board =
--(    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light) )
--( (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) )
--(    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light)    (White,Nothing) (Black,Just Light) )
--(    (Black,Nothing)    (White,Nothing)    (Black,Nothing)    (White,Nothing)    (Black,Nothing)    (White,Nothing)    (Black,Nothing)    (White,Nothing) )
--(    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark) )
--(  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing) )
--(    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark) )
--(  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing)  (Black,Just Dark)    (White,Nothing) )
--
newBoard :: Board
newBoard = Board {
    board = defaultBoard $ fromList 8 8 $ take (8*8) $ map (\x -> (x, Nothing)) colorCycle
}

-- | colorCycle
--
-- >>> colorCycle
-- [White,Black,White,Black,White,Black,White,Black,Black,White,Black,White,Black,White,Black,White,White,Black,White,Black,White,Black,White,Black,Black,White,Black,White,Black,White,Black,White,White,Black,White,Black,White,Black,White,Black,Black,White,Black,White,Black,White,Black,White,White,Black,White,Black,White,Black,White,Black,Black,White,Black,White,Black,White,Black,White]
--
colorCycle :: [Square]
colorCycle = concat $ map f $ reverse ([1..8] :: [Int])
  where
   f n
    | even n    = take 8 $ cycle [White, Black]
    | otherwise = take 8 $ cycle [Black, White]

-- | defaultBoard
--
defaultBoard :: Matrix (Square, Maybe Piece) -> Matrix (Square, Maybe Piece)
defaultBoard m = result
  where
    light  = foldl' (\acc n -> mapRow (\_ (sq,_) -> if sq == White then (sq, Nothing) else (sq, Just Light)) n acc) m [1..3]
    dark   = foldl' (\acc n -> mapRow (\_ (sq,_) -> if sq == White then (sq, Nothing) else (sq, Just Dark)) n acc) light [5..8]
    result = dark
