module Daimyo.NumberTheory.Log (
    logBasePairs
) where

import Data.List

logBasePairs b x y by =
    fst $ 
    foldl'
        (\(acc'list, prev) n ->
            let
                lb = logBase b n
            in
                (acc'list ++ [(prev, lb)], lb)
        )
        ([], logBase b x) [x+by,x+by+by..y]

t_logBasePairs'10 = logBasePairs 10 1 10 1
