{-
 - source: http://www.exploringbinary.com/ten-ways-to-check-if-an-integer-is-a-power-of-two-in-c/
 -}

module Daimyo.Math.Powers (
 isPowerOfTwo'DecAndCmp,
 isPowerOfTwo'Test
) where

import Data.Bits
import Data.List

isPowerOfTwo'DecAndCmp x = (x /= 0) && ((x .&. (x - 1)) == 0)

isPowerOfTwo'ComplementAndCompare x = (x /= 0) && ((x .&. (complement x + 1)) == x)

isPowerOfTwo'ShiftRight x
 | ((x .&. 1) == 0) && x > 1 = isPowerOfTwo'ShiftRight (x `shiftR` 1)
 | otherwise = x == 1

isPowerOfTwo'Test f = all f $ take 100 powersOfTwo

-- http://igm.univ-mlv.fr/~boutarel/okn/math/MathTools_8hpp-source.html

ceilPowerOfTwo n = 1 + foldl (\acc i -> acc .|. (acc `shiftR` i)) (n - 1) [1,2,4,8,16]

floorPowerOfTwo n = (ceilPowerOfTwo n) `shiftR` 1

powersOfTwo :: [Integer]
powersOfTwo = map ((^)2) [1..]

test = map (\f -> isPowerOfTwo'Test f) tests

tests =
 [
  isPowerOfTwo'DecAndCmp,
  isPowerOfTwo'ComplementAndCompare,
  isPowerOfTwo'ShiftRight
 ]

-- the nub way
powersOfTwo' =
 [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,
  65536,131072,262144,524288,1048576,2097152,4194304,8388608,
  16777216,33554432,67108864,134217728,268435456,536870912,
  1073741824,2147483648]
