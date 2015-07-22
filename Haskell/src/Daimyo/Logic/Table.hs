module Daimyo.Logic.Table (
    permutationTable
) where

import           Daimyo.Combinatorics.Combination
import           Daimyo.Logic.Proposition

import           Data.List
import           Data.Matrix

permutationTable n = fromLists $ n `chooseFrom` [T,F]
