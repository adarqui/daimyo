module Daimyo.NumberTheory.Benford (
    benford'law,
    benford'10,
    benford'26,
    ben
) where

import Data.Char

benford'law d = ben 10 d

benford'10 = benford'law

-- incorrect, just playing around
benford'26 c =
    let
        c' = ord $ toUpper c
        d = (fromIntegral c' :: Double) - 64
    in
        ben 26 d

ben base d = logBase base (1 + 1/d)
