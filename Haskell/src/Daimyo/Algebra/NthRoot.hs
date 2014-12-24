{-# LANGUAGE NPlusKPatterns #-}

module Daimyo.Algebra.NthRoot (
    nthRoot
) where

{-
    the nth root of k is: k^(1/r)
-}

nthRoot r k = k ** (1/r)
