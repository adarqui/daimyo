module Daimyo.Lib.Bits (
 IntB,
 new
) where

import Data.List
import qualified Data.Bits as Bits

data Tree = Empty | Node Tree !Int Tree deriving (Show, Eq, Ord)

newtype IntB = IntB Tree deriving (Show, Eq, Ord)

new :: Int -> IntB
new n = IntB (fromList $ balance [1..n])

fromList :: [Int] -> Tree
fromList [] = Empty
fromList (x:xs) =
    Node
        (fromList $ dropWhile (>= x) xs)
        x
        (fromList $ dropWhile (< x) xs)

balance l = intersperseL evens $ reverse odds
    where
        (odds, evens) = splits l

splits l =foldl' (\(odds,evens) (v,i) -> if (odd i) then (odds ++ [v], evens) else (odds, evens ++ [v])) ([],[]) $ zip l [1..]

intersperseL xs [] = xs
intersperseL [] ys = ys
intersperseL (x:xs) (y:ys) = x : y : intersperseL xs ys
