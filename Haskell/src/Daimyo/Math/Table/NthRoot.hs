module Daimyo.Math.Table.NthRoot (
    nthRootTable
) where

import Daimyo.Algebra.NthRoot
import qualified Data.Matrix as M

{-
    creates an nth root table from f to t, using pf to pt nth
-}

nthRootTable ft pfpt = M.fromLists $ nthRootTable' ft pfpt

nthRootTable' (f,t) (pf,pt) = map (\i -> map (\root -> nthRoot root i) [pf..pt]) [f..t]

t_nthRootTable = putStrLn $ show $ nthRootTable (1,33) (1,4)
t_nthRootTable' = putStrLn $ show $ nthRootTable (1,100) (1,5)
