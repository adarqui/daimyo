module Daimyo.Lib.Math.Tables (
    powTable
) where

import Control.Monad

{-
 creates a power table from f to t, using pf to pt power functions (x = f to t in x^pf, x^pf+1... x^pt)
-}

powTable (f,t) (pf,pt) = map (\i -> map (\p -> i^p) [pf..pt]) [f..t]

powEx = mapM_ (putStrLn . show) $ powTable (1,20) (1,6)
