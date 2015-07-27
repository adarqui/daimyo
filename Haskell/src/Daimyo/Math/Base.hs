module Daimyo.Math.Base (
    factorBase,
    translateFactorBase
) where

import Daimyo.Number
import Data.List

factorBase _ 0 = []
factorBase b n =
    let
        (q,r) = quotRem n b
    in
        (factorBase b q) ++ [r]


translateFactorBase b n =
    let
        facs = factorBase b n
        r'facs = zip (reverse facs) oneBy10
    in
        sum $ map (\(b,p) -> b*p) r'facs


t_stuff = [factorBase 2 12, factorBase 2 64, factorBase 2 156, factorBase 10 23, factorBase 10 230, factorBase 8 23, factorBase 16 42]
