module Daimyo.Game.Checkers (
    Piece (..),
    Square (..),
    Board (..),
    newBoard,
    defaultBoard
) where

import Data.Matrix
import Data.List

{-
    TODO: RULES, SIM, etc
-}

{-
    source:

        http://simple.wikipedia.org/wiki/Checkers
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

data Piece = Light | Dark deriving (Show, Eq)

data Square = White | Black deriving (Show, Eq, Enum)

data Board = Board {
    board :: Matrix (Square, Maybe Piece)
} deriving (Show)

newBoard = Board {
    board = defaultBoard $ fromList 8 8 $ take (8*8) $ map (\x -> (x, Nothing)) color'cycle
}

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


defaultBoard :: Matrix (Square, Maybe Piece) -> Matrix (Square, Maybe Piece)
defaultBoard m =
    let
        light = foldl' (\acc n -> mapRow (\_ (sq,p) -> if sq == White then (sq, Nothing) else (sq, Just Light)) n acc) m [1..3]
        dark = foldl' (\acc n -> mapRow (\_ (sq,p) -> if sq == White then (sq, Nothing) else (sq, Just Dark)) n acc) light [5..8]
        result = dark
    in
        result
