module Daimyo.Sorting.Radix.Array (
  rsort
) where

import           GHC.Arr

type Key v     = [v]
type Bucket v  = [Key v]
type Buckets v = Array v (Bucket v)

-- | rsort
--
-- >>> rsort 3 ('a','z') ["abc", "def", "zkg", "acd", "zbb", "bce", "bec", "zac"]
-- ["abc","acd","bce","bec","def","zac","zbb","zkg"]
--
rsort :: Ix a => Int -> (a, a) -> [Key a] -> [Key a]
rsort 0 _ l       = l
rsort p _bounds l = rsort p' _bounds (concatA (split p' _bounds l))
  where
    p' = p - 1

-- | split
--
split :: Ix a => Int -> (a, a) -> [Key a] -> Buckets a
split k _bounds l = accumArray f [] _bounds [ (x!!k,x) | x <- l]
  where
    f l key = key : l

-- | concatA
--
concatA :: Ix a => Buckets a -> [Key a]
concatA = foldr rev [] . elems
  where
    rev [] acc     = acc
    rev (x:xs) acc = rev xs (x:acc)
