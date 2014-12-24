module Daimyo.List.Misc (
    subseqs,
    subsequences',
    interleave,
    rests
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
