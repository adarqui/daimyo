module Daimyo.Math.Table.Power (
    powTable,
    powTable'
) where

import Control.Monad
import qualified Data.Matrix as M

{-
 creates a power table from f to t, using pf to pt power functions (x = f to t in x^pf, x^pf+1... x^pt)
-}

powTable ft pfpt = M.fromLists $ powTable' ft pfpt

powTable' (f,t) (pf,pt) = map (\i -> map (\p -> i^p) [pf..pt]) [f..t]

t_powTable = putStrLn $ show $ powTable (1,20) (1,6)
