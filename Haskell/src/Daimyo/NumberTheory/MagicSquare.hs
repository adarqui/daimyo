module Daimyo.NumberTheory.MagicSquare (
    magicSquare,
    magicSquare'18x19
) where

import Daimyo.Number
import Data.Matrix

magicSquare = undefined

{-
TODO FIXME

Decimal portions get wrecked from my decimalPartToInteger. 1/19 should be 05.., not 5..

The decimal periods of 1/19, 2/19, . . . , 18/19, however, do form a
true magic square.
1/19 = 052631578947368421
2/19 = 105263157894736842
3/19 = 157894736842105263
4/19 = 210526315789473684
5/19 = 263157894736842105
6/19 = 315789473684210526
7/19 = 368421052631578947
8/19 = 421052631578947368
9/19 = 473684210526315789
10/19 = 526315789473684210
11/19 = 578947368421052631
12/19 = 631578947368421052
13/19 = 684210526315789473
14/19 = 736842105263157894
15/19 = 789473684210526315
16/19 = 842105263157894736
17/19 = 894736842105263157
18/19 = 947368421052631578
-}

magicSquare'18x19 =
    let
        recips = map (\n -> show $ decimalPartToInteger (n/19)) [1..18]
    in
        fromLists recips
