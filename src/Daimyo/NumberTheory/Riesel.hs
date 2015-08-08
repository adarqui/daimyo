module Daimyo.NumberTheory.Riesel (
    riesel'relative,
    riesel'list,
    riesel'
) where

import Daimyo.NumberTheory.Prime
import Data.List

riesel'relative k n = all (\(r,b) -> b == True) $ riesel'list k n

riesel'list k n = map (\n -> let r = riesel' k n in (r, isComposite r)) [1..n]

riesel' k n = k*(2^n)-1

t_riesel'true = map (\k -> riesel'relative k 11) [2293, 9221, 23669, 31859, 38473, 46663, 509203]
t_riesel'false = map (\k -> riesel'relative k 11) [10, 23]
