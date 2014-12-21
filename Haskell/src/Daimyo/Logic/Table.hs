module Daimyo.Logic.Table (
    permutationTable
) where

import Daimyo.Logic.Proposition
import Daimyo.Combinatorics.Combination

import Data.List
import Data.Matrix

permutationTable n = fromLists $ n `chooseFrom` [T,F]

t_permutationTable = permutationTable 2
