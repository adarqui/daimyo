module Daimyo.Lib.List.Misc (
 subseqs
) where

import Data.List

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]
