module Daimyo.Bits (
  IntB,
  new,
  charToBits,
  intersperseL,
  splits,
  balance,
  fromList
) where

import qualified Data.Bits as Bits
import           Data.List

-- | Tree
--
data Tree
  = Empty
  | Node Tree !Int Tree
  deriving (Show, Eq, Ord)

-- | IntB
--
newtype IntB
  = IntB Tree
  deriving (Show, Eq, Ord)

-- | new
--
-- wat?
--
-- >>> new 5
-- IntB (Node (Node Empty 1 Empty) 2 (Node (Node (Node (Node Empty 1 Empty) 3 Empty) 4 Empty) 5 Empty))
--
new :: Int -> IntB
new n = IntB (fromList $ balance [1..n])

-- | fromList
--
-- >>> fromList [1..5]
-- Node Empty 1 (Node Empty 2 (Node Empty 3 (Node Empty 4 (Node Empty 5 Empty))))
--
fromList :: [Int] -> Tree
fromList []     = Empty
fromList (x:xs) =
  Node
    (fromList $ dropWhile (>= x) xs)
    x
    (fromList $ dropWhile (< x) xs)

-- | balance
--
-- >>> balance [1..10] :: [Int]
-- [2,9,4,7,6,5,8,3,10,1]
--
balance :: [t] -> [t]
balance l = intersperseL evens $ reverse odds
  where
    (odds, evens) = splits l

-- | splits
--
-- >>> splits [1..10] :: ([Int],[Int])
-- ([1,3,5,7,9],[2,4,6,8,10])
--
splits :: [a] -> ([a], [a])
splits l = foldl' f ([],[]) $ zip l [1..]
  where
    f (odds, evens) (v, i)
      | odd i     = (odds ++ [v], evens)
      | otherwise = (odds, evens ++ [v])

-- | intersperseL
--
-- >>> intersperseL [5,6,7] [1,2,3] :: [Int]
-- [5,1,6,2,7,3]
--
intersperseL xs [] = xs
intersperseL [] ys = ys
intersperseL (x:xs) (y:ys) = x : y : intersperseL xs ys

-- | charToBits
--
-- shouldn't this take a Char? so sad.
--
-- charToBits 65
-- [1,0,0,0,0,0,1,0]
--
charToBits :: Int -> [Int]
charToBits char = map f [0..7]
  where
    f bit
      | Bits.testBit char bit = 1
      | otherwise             = 0
