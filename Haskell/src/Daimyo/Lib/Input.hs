module Daimyo.Lib.Input (
 getNums,
 getNumTup
) where

import Data.List
import Data.Char

getNums s = map read . words $ s

getNumTup s = let w = words s in (read (w !! 0), read (w !! 1))
