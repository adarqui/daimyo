module Daimyo.NumberTheory.Sierpinski (
    sierpinski'relative,
    sierpinski'list,
    sierpinski'
) where

import Daimyo.NumberTheory.Prime
import Data.List

sierpinski'relative k n = all (\(r,b) -> b == True) $ sierpinski'list k n

sierpinski'list k n = map (\n -> let r = sierpinski' k n in (r, isComposite r)) [1..n]

sierpinski' k n = k*(2^n)+1

t_sierpinski'true = map (\k -> sierpinski'relative k 11) [78557, 271129, 271577, 322523, 327739, 482719, 575041, 603713, 903983, 934909, 965431]
t_sierpinski'false = map (\k -> sierpinski'relative k 11) [10, 23]
