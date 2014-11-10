{- info: http://en.wikipedia.org/wiki/Insertion_sort -}

module Daimyo.Lib.Sorting.Insert (
 isort'R,
 isort'R'p,
 isort'L,
 isort'L'p
) where

import Data.List

{-
insert O(N)
foldr O(N)
total: O(N^2)
-}
isort'R :: Ord a => [a] -> [a]
isort'R = foldr (\e acc -> insert e acc) []

isort'R'p :: Ord a => [a] -> [[a]]
isort'R'p = reverse . scanr (\e acc -> insert e acc) []

isort'L :: Ord a => [a] -> [a]
isort'L = foldl' (\acc e -> insert e acc) []

isort'L'p :: Ord a => [a] -> [[a]]
isort'L'p = scanl (\acc e -> insert e acc) []

{-
 grouped isort: stores everything as (a,[a..aN]) pairs
 > isort'Grouped'L'go [] [9,9,9,9,1,1,2,2,3,3,3,3,3]
 [(1,[1]),(2,[2]),(3,[3,3,3,3]),(9,[9,9,9])]
-}
isort'Grouped'L :: Ord a => [a] -> [a]
isort'Grouped'L l = concat $ foldl' (\acc (a,r) -> acc ++ [a : r]) [] $ isort'Grouped'L'go [] l

isort'Grouped'L'go :: Ord a => [(a,[a])] -> [a] -> [(a,[a])]
isort'Grouped'L'go acc [] = acc
isort'Grouped'L'go acc (x:xs) = isort'Grouped'L'go ins (dropWhile (==x) xs)
 where
  ins = insertBy (\(a,_) (b,_) -> compare a b) (x,takeWhile (==x) xs) acc 


{-
 grouped isort: stores everything as (a,N) pairs
 > isort'Grouped'Condensed'L'go [] ((replicate 1000 5)++(replicate 1000 6)++[7])
 [(5,1000),(6,1000),(7,1)]
-}

isort'Grouped'Condensed'L :: Ord a => [a] -> [a]
isort'Grouped'Condensed'L l = foldl' (\acc (a,n) -> acc ++ replicate n a) [] $ isort'Grouped'Condensed'L'go [] l

isort'Grouped'Condensed'L'go :: Ord a => [(a,Int)] -> [a] -> [(a,Int)]
isort'Grouped'Condensed'L'go acc [] = acc
isort'Grouped'Condensed'L'go acc (x:xs) = isort'Grouped'Condensed'L'go ins (dropWhile (==x) xs)
 where
  ins = insertBy (\(a,_) (b,_) -> compare a b) (x,1+(length $ takeWhile (==x) xs)) acc 
