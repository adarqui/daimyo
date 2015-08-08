module Daimyo.Algebra.Congruence.Solving (
    solutionIsUniqueModN,
    solveSystemCongruence,
    findEqIntegers,
    findEq,
    eq
) where

import           Daimyo.Number
import           Data.List
import           Data.Maybe

-- | solutionIsUniqueModN
--
-- >>> solutionIsUniqueModN 5 7
-- True
--
-- >>> solutionIsUniqueModN 4 8
-- False
--
solutionIsUniqueModN :: Integral a => a -> a -> Bool
solutionIsUniqueModN a n = gcd a n == 1

{-
    ax = b (mod n)
    ie,
        24x = 23 (mod n)
        x = -207 = 10 (mod 31)

    system is in the form: a b n === ax = b (mod n)
-}

-- | solveSystemCongruence
--
-- >>> take 20 $ solveSystemCongruence 24 23 31
-- [-207.0,-920.0,-1633.0,-2346.0,-3059.0,-3772.0,-4485.0,-5198.0,-5911.0,-6624.0,-7337.0,-8050.0,-8763.0,-9476.0,-10189.0,-10902.0,-11615.0,-12328.0,-13041.0,-13754.0]
--
solveSystemCongruence :: (RealFrac t, Enum t) => t -> t -> t -> [t]
solveSystemCongruence a b n = map (\(x,y) -> b*x) $ findEqIntegers a n

eq :: (Fractional t, Eq t) => t -> t -> t -> Maybe (t, t)
eq a n t =
    let
        s = 1/a - ((n*t) / a)
        r = a*s + n*t
    in
        if (r == 1)
            then Just (s, t)
            else Nothing

findEqIntegers :: (RealFrac t, Enum t) => t -> t -> [(t, t)]
findEqIntegers a n = filter (\(x,y) -> isInteger x) $ findEq a n

findEq :: (Fractional a, Eq a, Enum a) => a -> a -> [(a, a)]
findEq a n = catMaybes $ map (eq a n) [1..]

{-
    test:
        24s + 31t = 1
        s + 31t/24 = 1/24
        s = 1/24 - 31t/24
-}

{-
t_s t = 1/24 - (31*(t) / 24)

t_system'1 = take 20 $ solve'system'congruence 24 23 31
t_system'2 = take 20 $ solve'system'congruence 9 1 36 -- <-- NO SOLUTION
t_system'3 = take 20 $ solve'system'congruence 1 14 31
t_system'4 = take 20 $ solve'system'congruence 1 6 31
-}
