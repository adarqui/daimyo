module Daimyo.Pearls.SmallestFreeNumber (
  findSmallestList,
  findSmallestArray,
  findSmallestArrayST,
  findSmallestDC,
  minFree,
  (\\\),
  search,
  checkList,
  checkListST,
  countList
) where

import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.Maybe

-- | findSmallestList
--
-- >>> findSmallestList [1,5,99,50,3,6,7,10,2,0]
-- 4
--
findSmallestList :: [Int] -> Int
findSmallestList = minFree

-- | findSmallestArray
--
-- >>> findSmallestArray [1,5,99,50,3,6,7,10,2,0]
-- 4
--
findSmallestArray :: [Int] -> Int
findSmallestArray = fst . fromJust . find (not . snd) . assocs . checkList

-- | findSmallestArrayST
--
-- >>> findSmallestArrayST [1,5,99,50,3,6,7,10,2,0]
-- 4
--
findSmallestArrayST :: [Int] -> Int
findSmallestArrayST = fst . fromJust . find (not . snd) . assocs . checkListST

-- | minFree
--
-- >>> minFree [0,1,4,3,5] :: Int
-- 2
--
minFree :: (Num a, Eq a, Enum a) => [a] -> a
minFree xs = head ([0..] \\\ xs)

-- | \\\
--
-- >>> [1,2,3,4,5] \\\ [1,2,4,6] :: [Int]
-- [3,5]
--
(\\\) :: Eq a => [a] -> [a] -> [a]
us \\\ vs = filter (not . flip elem vs) us

-- search
--
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- | checklist
--
-- >>> checkList [1,2,3,5]
-- array (0,4) [(0,False),(1,True),(2,True),(3,True),(4,False)]
--
checkList :: [Int] -> Array Int Bool
checkList xs =
  let n = length xs in
  accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))

-- | countList
--
-- >>> countList [0,1,2,3,5,5,5]
-- array (0,7) [(0,1),(1,1),(2,1),(3,1),(4,0),(5,3),(6,0),(7,0)]
--
countList :: [Int] -> Array Int Int
countList xs = let n = length xs in accumArray (+) 0 (0, n) (zip xs (repeat 1))

-- | checkListST
--
-- >>> checkListST [1,2,3,5]
-- array (0,4) [(0,False),(1,True),(2,True),(3,True),(4,False)]
checkListST :: [Int] -> Array Int Bool
checkListST xs =
  runSTArray $ do
    let n = length xs
    a <- newArray (0, n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a

-- | findSmallestDC
--
-- divide and conquer
--
-- >>> findSmallestDC [1,5,99,50,3,6,7,10,2,0]
-- 4
--
findSmallestDC :: [Int] -> Int
findSmallestDC = minFree'

minFree' xs = minFrom 0 (length xs, xs)
minFrom a (n, xs)
  | n == 0 = a
  | m == b - a = minFrom b (n-m, vs)
  | otherwise = minFrom a (m, us)
  where
    (us, vs) = partition (<b) xs
    b = a + 1 + n `div` 2
    m = length us

difference_proof as bs cs = [t1,t2,t3]
  where
    t1 = ((as ++ bs) \\ cs) == ((as \\ cs) ++ (bs \\ cs))
    t2 = (as \\ (bs ++ cs)) == ((as \\ bs) \\ cs)
    t3 = ((as \\ bs) \\ cs) == ((as \\ cs) \\ bs)
