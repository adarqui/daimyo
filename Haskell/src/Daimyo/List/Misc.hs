module Daimyo.List.Misc (
    subseqs,
    subsequences',
    interleave,
    rests,
    removeIndices
) where

import Data.List

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

subsequences' l = filter (not . null) $ subsequences l

{-
    rests
    [2,2,2,3]
    ([2],[2,2,3])
    ([2,2],[2,3])
    ([2,3],[2,2])
    ([2,2,2],[3])
    ..
-}

rests l =
    let
        perms = permutations l
    in
        nub $ concatMap (\p -> map (\n -> (take n p, drop n p)) [1..(length l)-1]) perms


{-
    Removes elements found in the list of indexes
    Returns a tuple: (removed items, new list)
-}

removeIndices :: [Int] -> [a] -> ([a],[a])
removeIndices indices list = removeIndices' 0 ([],[]) (sort $ nub indices) list

removeIndices' :: Int -> ([a],[a]) -> [Int] -> [a] -> ([a], [a])
removeIndices' _ (acc,acc') [] list = (acc,acc' ++ list)
removeIndices' _ (acc,acc') _ [] = (acc,acc')
removeIndices' iter (acc,acc') indices@(i:is) list@(x:xs)
    | iter == i = removeIndices'(iter+1) (acc++[x],acc') is xs
    | otherwise = removeIndices' (iter+1) (acc,acc'++[x]) indices xs
    
t_removeIndieces =
    let
        s = "hello world"
        ri = removeIndices
    in
        [ri [0,1,2] s, ri [1,3] s, ri [6] s, ri [] s, ri [100] s, ri [100] []]
