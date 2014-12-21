module Daimyo.Algebra.Congruence.Solving (
    solution'is'unique'modn,
    solve'system'congruence,
    find'eq'Integers,
    find'eq,
    eq
) where

import Data.Maybe

solution'is'unique'modn a n = gcd a n == 1

{-
    ax = b (mod n)
    ie,
        24x = 23 (mod n)
        x = -207 = 10 (mod 31)

    system is in the form: a b n === ax = b (mod n)
-}

solve'system'congruence a b n = map (\(x,y) -> b*x) $ find'eq'Integers a n


eq a n t =
    let
        s = 1/a - ((n*t) / a)
        r = a*s + n*t
    in
        if (r == 1)
            then Just (s, t)
            else Nothing

find'eq'Integers a n = filter (\(x,y) -> floor x == ceiling x) $ find'eq a n

find'eq a n = catMaybes $ map (eq a n) [1..]


{-
    test:
        24s + 31t = 1
        s + 31t/24 = 1/24
        s = 1/24 - 31t/24
-}

t_s t = 1/24 - (31*(t) / 24)

t_system'1 = take 20 $ solve'system'congruence 24 23 31
t_system'2 = take 20 $ solve'system'congruence 9 1 36 -- <-- NO SOLUTION
t_system'3 = take 20 $ solve'system'congruence 1 14 31
t_system'4 = take 20 $ solve'system'congruence 1 6 31

