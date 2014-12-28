module Daimyo.Algebra.Divisibility.Examples (
) where

import Daimyo.Algebra.Divisibility

{-

    divisibility algorithm:

    5 `divides` 13

    a = 13
    b = 5
    q = 2
    r = 3

    13 = 5 * 2 + 3
-}

t_division'1 = division'algorithm 13 5 == (2, 3)

{-
    euclidean algorithm:


    Problem:

    d = GCD (13, 5)

    13 = 5 * 2 + 3
     5 = 3 * 1 + 2
     3 = 2 * 1 + [1]
     2 = 1 * 2 + 0


    Problem:

    d = GCD (162, 36)

    162 = 36 * 144 + [18]
     36 = 18 * 2 + 0


    Problem:

    d = GCD (2045, 175)

    2045 = 175 * 11 + 120
     175 = 120 * 1  + 55
     120 = 55 * 2 + 10
      55 = 10 * 5 + [5]
      10 = 5 * 10

-}

t_gcd = [gcd 13 5 == 1, gcd 162 36 == 18, gcd 2045 175 == 5]
