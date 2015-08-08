module Daimyo.Optimizations.List (
  insert1,
  insert2,
  filter1,
  filter2,
  filter3,
  reverse1,
  reverse2
) where

-- | insert1 vs insert2
--
-- insert2 saves a 'cons' by using a label (l@)
--

-- | filter1 vs filter2
--
-- filter2 is tail recursive and tail strict
--  entire list will have to be traversed
--  less stack/heap space used
-- filter1 performs better in a lazy pipeline
--

-- | reverse1 vs reverse2
--
-- reverse2 removes the un-needed append (++) found in reverse1
--  an improvement from O(n^2) (reverse1) to O(n) (reverse2)
--

-- | insert1
--
insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (y:ys)
  | x > y     = y : insert1 x ys
  | otherwise = x:y:ys

-- | insert2
--
insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x l@(y:ys)
  | x > y     = y : insert2 x ys
  | otherwise = x:l

-- | filter1
--
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 p (x:xs)
  | p x       = x : filter1 p xs
  | otherwise = filter1 p xs

-- | filter2
--
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p xs = go p xs []
  where
    go _ [] r     = reverse r
    go p' (x:xs') r
      | p' x      = go p' xs' (x:r)
      | otherwise = go p' xs' r

-- | filter3
--
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 p l = foldr f [] l
  where
    f x acc = if p x
                 then (x:acc)
                 else acc

-- | reverse1
--
reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse xs ++ (x:[])

-- | reverse2
--
reverse2 :: [a] -> [a]
reverse2 l = go l []
  where
    go [] y     = y
    go (x:xs) y = go xs (x:y)
