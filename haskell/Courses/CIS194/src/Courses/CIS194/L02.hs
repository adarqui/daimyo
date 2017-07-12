-- | Lesson 02 from CIS194
module Courses.CIS194.L02 (
  Peg (..),
  Code,
  Move (..),
  colors,
  exactMatches,
  countColors,
  matches,
  getMove,
  isConsistent,
  filterCodes
) where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- | Get the number of exact matches between the actual code and the guess
-- >>> exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red]
-- 0
--
-- >>> exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange]
-- 2
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (secret:ss) (guess:gs) = v + exactMatches ss gs
  where
    v = if (secret == guess) then 1 else 0

-- Exercise 2 -----------------------------------------

-- | For each peg in xs, count how many times is occurs in ys
--
-- >>> countColors [Red, Blue, Yellow, Purple]
-- [1, 0, 1, 1, 0, 1]
--
-- >>> countColors [Green, Blue, Green, Orange]
-- [0, 2, 1, 0, 1, 0]
--
-- >>> (,) (countColors [Red, Blue, Yellow, Orange]) (countColors [Red, Orange, Orange, Blue])
-- ([1,0,1,1,1,0],[1,0,1,0,2,0])
countColors :: Code -> [Int]
countColors input = map (\color -> length $ filter (== color) input) colors

-- | Count number of matches between the actual code and the guess
--
-- >>> matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- 3
matches :: Code -> Code -> Int
matches secrets guesses = foldr (+) 0 $ zipWith min (countColors secrets) (countColors guesses)

-- Exercise 3 -----------------------------------------

-- | Construct a Move from a guess given the actual code
--
-- >>> getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- Move [Red,Orange,Orange,Blue] 1 2
getMove :: Code -> Code -> Move
getMove secrets guesses = Move guesses exact non_exact
  where
    exact = exactMatches secrets guesses
    non_exact' = matches secrets guesses - exact
    non_exact = if non_exact' < 0 then 0 else non_exact'

-- Exercise 4 -----------------------------------------

-- | isConsistent
--
-- >>> isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple]
-- True
--
-- >>> isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple]
-- False
isConsistent :: Move -> Code -> Bool
isConsistent (Move guesses exact non_exact) provided = exact == exact' && non_exact == non_exact'
  where
    (Move guesses' exact' non_exact') = getMove guesses provided

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = carve n colors

carve :: Int -> [a] -> [[a]]
carve n codes = fst $ break (\xs -> length xs > n) $ myPerms codes [[]]

-- [[Red],[Blue],[Green],[Red,Red],[Red,Blue],[Red,Green],[Blue,Red],[Blue,Blue],[Blue,Green],[Green,Red],[Green,Blue],[Green,Green],...]
myPerms :: [a] -> [[a]] -> [[a]]
myPerms [] _ = []
myPerms codes acc = concated ++ myPerms codes concated
  where
    concated = [ xs : ys | xs <- codes, ys <- acc ]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
