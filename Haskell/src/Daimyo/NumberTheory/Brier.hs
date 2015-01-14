module Daimyo.NumberTheory.Brier (
    brier'relative,
    brier'list,
    brier'
) where

import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Riesel
import Daimyo.NumberTheory.Sierpinski
import Data.List

brier'relative k n = all (\(r,s,c) -> c == True) $ brier'list k n

brier'list k n = map (\n -> brier' k n) [1..n]

brier' k n =
    let
        r = riesel' k n
        s = sierpinski' k n
    in
        (r, s, isComposite r && isComposite s)

t_brier = map (\k -> brier'relative k 2) [878503122374924101526292469, 3872639446526560168555701047, 623506356601958507977841221247]
