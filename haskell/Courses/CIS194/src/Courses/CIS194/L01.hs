-- | Lesson 01 from CIS194
module Courses.CIS194.L01 (
  -- ** Exercise 1
  lastDigit,
  dropLastDigit,
  -- ** Exercise 2
  toRevDigits,
  -- ** Exercise 3
  doubleEveryOther,
  -- ** Exercise 4
  sumDigits,
  -- ** Exercise 5
  luhn,
  -- ** Exercise 6
  Peg,
  Move,
  hanoi,
  -- ** Exercise 7
  hanoi4
) where

import Data.List (foldl')

-- | lastDigit: Return the last digit of a number.
--
-- >>> lastDigit 0
-- 0
--
-- >>> lastDigit 1
-- 1
--
-- >>> lastDigit 12
-- 2
-- >>> lastDigit 123
-- 3
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- | dropLastDigit: Remove the last digit of a number.
--
-- >>> dropLastDigit 0
-- 0
--
-- >>> dropLastDigit 1
-- 0
--
-- >>> dropLastDigit 12
-- 1
--
-- >>> dropLastDigit 123
-- 12
dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - (lastDigit n)) `div` 10

-- | toRevDigits: Given an `Integer`, return a reversed list of its' digits.
--
-- >>> toRevDigits 0
-- []
--
-- >>> toRevDigits 1
-- [1]
--
-- >>> toRevDigits 1234
-- [4,3,2,1]
--
-- >>> toRevDigits (-1)
-- []
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n > 0 = revLoop n
  | otherwise = []
  where
    revLoop :: Integer -> [Integer]
    revLoop 0 = []
    revLoop n = lastDigit n : revLoop (dropLastDigit n)

-- | doubleEveryOther: Doubles every other number in the list.
--
-- >>> doubleEveryOther [4,9,5,5]
-- [4,18,5,10]
--
-- >>> doubleEveryOther [0,0]
-- [0,0]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther numbers = map (uncurry (*)) $ zip numbers $ cycle [1,2]

-- | sumDigits: Calculate the sum of all the digits in an Integer.
--
-- >>> sumDigits [10, 5, 18, 4]
-- 19
sumDigits :: [Integer] -> Integer
sumDigits numbers = foldl' (+) 0 $ concatMap toRevDigits numbers

-- | luhn: Validates credit card numbers.
--
-- >>> luhn 5594589764218858
-- True
--
-- >>> luhn 1234567898765432
-- False
--
-- Luhn Algorithm
--
-- Double every value of every second digit beginning from the right.
-- Add the digits of the doubled values and the undoubled digits from the original number.
-- Calculate the remainder when the sum is divided by 10.
-- If the result equals 0, then the number is valid.
luhn :: Integer -> Bool
luhn card_number = (sumDigits $ doubleEveryOther $ toRevDigits card_number) `mod` 10 == 0

-- | Hanoi type synonyms
type Peg = String
type Move = (Peg, Peg)

-- | hanoi: Solve the Towers of Hanoi puzzle for three pegs.
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"), ("a","b"), ("c","b")]
--
-- Towers of Hanoi
--
-- Discs of different sizes are stacked
-- on three pegs; the goal is to get from a starting configuration with
-- all discs stacked on the first peg to an ending configuration with all
-- discs stacked on the last peg, as shown in Figure 3.
--
-- The Towers of Hanoi objective
-- The only rules are
-- • you may only move one disc at a time, and
-- • a larger disc may never be stacked on top of a smaller one.
-- For example, as the first move all you can do is move the topmo
hanoi :: Integer -- ^ Number of discs on the starting peg
      -> Peg     -- ^ First Peg
      -> Peg     -- ^ Second Peg
      -> Peg     -- ^ Third Peg
      -> [Move]  -- ^ The result moves made to solve the puzzle
hanoi num_discs peg1 peg2 peg3 = undefined

hanoi4 = undefined
