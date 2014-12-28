module Daimyo.NumberTheory.Nines (
    nines
) where

import Daimyo.Number
import Data.List

nines n
    | n < 10 = if (n == 9) then 0 else n
    | otherwise =
        let
            d = digits n
        in
            nines (sum $ map (\x -> if (x == 9) then 0 else x) d)

t_nines'1 =
    let
        l = [3264, 8415, 2946, 3206]
        v = map nines l
    in
        (nines $ sum l, nines $ sum v, v)

t_nines'2 =
    let
        l = [5643, 2891]
        v = map nines l
    in
        (nines $ abs $ foldl' (-) 0 l, nines $ abs $ foldl' (-) 0 v, v)

t_nines'3 =
    let
        l = [548, 629]
        v = map nines l
    in
        (nines $ product l, nines $ product v, v)
