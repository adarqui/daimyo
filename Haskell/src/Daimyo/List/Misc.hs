module Daimyo.List.Misc (
    subseqs,
    subsequences'
) where

import Data.List

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

subsequences' l = filter (not . null) $ subsequences l
