module Daimyo.PSS.C1.E2 (
    blackboard,
    problem,
    solution
) where

import Daimyo.Random
import Daimyo.List.Misc

{-
E2. Suppose the positive integer n is odd. First Al writes the numbers 1, 2,..., 2n
on the blackboard. Then he picks any two numbers a, b, erases them, and writes,
instead, |a − b|. Prove that an odd number will remain at the end.
-}

solution n
    | n >= 1 && odd n = abs $ 1 - 2*n
    | otherwise = error "n must be >= 1 and odd"

{-
    Still need to prove this mathematically.. hehe.. see above perhaps?
-}

blackboard n
    | even n = error "n must be odd"
    | otherwise = [1..2*n]

problem n =
    problem' (blackboard n)

problem' (b:[]) = b
problem' bb =
    let
        len = length bb
        (a, b) = getPairNe (intToW64 len)
        (a', b') = (w64ToInt a, w64ToInt b)
        (items, rest) = removeIndices [a', b'] bb
        (a'', b'') = let (x:y:_) = items in (x, y)
    in
        problem' $ (abs $ a'' - b'') : rest
