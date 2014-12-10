module Daimyo.Lib.Wave.Combinatorics.Examples (
) where

import Daimyo.Lib.Wave.Combinatorics.Permutation

import Data.List

{-
    Trotter
-}

{- C2 -}

{- Ex 2.1
. In the state of Georgia, license plates consist of four digits followed by a space followed by three capital letters. The first digit cannot be a 0. How many license plates are possible?
-}

ex2_1 =
    let
        x = ['0'..'9']
        y = " "
        z = ['A'..'Z']
        p = map length $ (tail x) : x : x : x : y : z : z : z : []
    in
        (product p, p)

ex2_1' =
    let
        x = 9 * (10^3)
        y = 1 * (1^1)
        z = 26^3
    in
        x * y * z

{-
    A machine instruction in a 32-bit operating system is just a bit string of length 32. So the number of such strings is 232 = 4294967296.
-}

ex2_2 =
    let
        x = [0,1]
        bs = replicate 32 x
    in
        product $ map length bs


{-
    It's time to elect a slate .. {President, Vice President, Secretary, Treasurer} among 80 students. How many different slates of officers can be elected?
-}
ex2_5 = permutations' (80, 4)
