module Daimyo.Chess (
    Piece (..),
    Square (..),
    Board (..),
    board
) where

import Data.Matrix

{-
    source:

        http://en.wikipedia.org/wiki/Chessboard
        http://en.wikipedia.org/wiki/Chess_piece
-}

{-
        a      b      c      d      e      f      g      h
    8 white, black, white, black, white, black, white, black
    7 black, white, black, white, black, white, black, white
    ...
    2 white, black, white, black, white, black, white, black
    1 black, white, black, white, black, white, black, white
        a      b      c      d      e      f      g      h     
-}

data Piece = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)

data Square = White | Black deriving (Show, Eq, Enum)

data Board = Board {
    layout :: Matrix Square,
    pieces :: Matrix (Maybe Piece)
} deriving (Show)

board = Board {
    layout = fromList 8 8 $ take (8*8) $ color'cycle,
    pieces = fromList 8 8 $ take (8*8) $ repeat Nothing
}

-- TODO: add default board setup with pieces & rules, etc.

color'cycle =
    concat $
    map
        (\n ->
            if (even n)
                then
                    take 8 $ cycle [White, Black]
                else
                    take 8 $ cycle [Black, White]
        )
    $ reverse [1..8]
