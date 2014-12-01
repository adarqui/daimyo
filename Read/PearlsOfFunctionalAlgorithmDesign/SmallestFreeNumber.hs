module PearlsOfFunctionalAlgorithmDesign.SmallestFreeNumber (
) where

import Data.List
import Data.Array
import Data.Array.ST
import Data.Maybe

{-
    Array/List Based
-}

findSmallest'List = minFree
findSmallest'Array = fst . fromJust . find (not . snd) . assocs . checkList
findSmallest'Array'ST = fst . fromJust . find (not . snd) . assocs . checkList'ST

minFree xs = head ([0..] \\\ xs)

us \\\ vs = filter (not . flip elem vs) us

-- what i normally would have done
ug \\\\ ly = filter (\i -> not $ elem i ly) ug

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checkList :: [Int] -> Array Int Bool
checkList xs =
    let n = length xs in
    accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))

countList :: [Int] -> Array Int Int
countList xs = let n = length xs in accumArray (+) 0 (0, n) (zip xs (repeat 1))

checkList'ST xs =
    runSTArray $ do
        let n = length xs
        a <- newArray (0, n) False
        sequence [writeArray a x True | x <- xs, x <= n]
        return a

minFree_worstCase xs = let n = length xs in ((n^2 + n)) `div` 2

t_minFree = minFree $ delete 30 [0..50]

t_set = delete 100 [0..1000]
t_findSmallest'List = findSmallest'List t_set
t_findSmallest'Array = findSmallest'Array t_set
t_findSmallest'Array'ST = findSmallest'Array'ST t_set
t_find = [t_findSmallest'List, t_findSmallest'Array, t_findSmallest'Array'ST]

{-
    Divide and Conquer
-}

findSmallest'DC = minFree'

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

t_findSmallest'DC = findSmallest'DC t_set
