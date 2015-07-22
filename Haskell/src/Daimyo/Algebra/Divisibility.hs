module Daimyo.Algebra.Divisibility (
  n2de,
  de2n,
  numberToDecimalExpansion,
  decimalExpansionToNumber,
  isEven,
  divisibleBy2,
  divisibleBy5,
  divisibleBy3,
  divisibleBy9,
  fractionToDe,
  divides,
  dividesF,
  unit,
  isNonTrivial,
  divisionAlgorithm,
  divisionAlgorithmShow,
  euclideanAlgorithm,
  divisors,
  properDivisors
) where

import           Daimyo.Algebra.Fraction
import           Daimyo.NumberTheory.Prime
import           Daimyo.String
import           Text.Printf

-- | numberToDecimalExpansion
--
-- >>> numberToDecimalExpansion (1234 :: Int)
-- [1,2,3,4]
--
numberToDecimalExpansion :: (Show a, Integral a) => a -> [Int]
numberToDecimalExpansion = map (read . flip (:) []) . show

-- | decimalExpansionToNumber
--
-- >>> decimalExpansionToNumber [1,2,3,4]
-- 1234
--
decimalExpansionToNumber :: [Int] -> Int
decimalExpansionToNumber = sum . flip (zipWith (\n k -> n*(10^k))) [0..] . reverse

n2de = numberToDecimalExpansion
de2n = decimalExpansionToNumber

-- | fractionToDe
--
fractionToDe :: (Show a, Fractional a) => a -> a -> ([Int], [Int])
fractionToDe num den =
  let
    (r, d) = split '.' (show (num/den))
    map' xs = map (\n' -> read (n':[]) :: Int) xs
  in
    (map' r, map' d)

-- | isEven   So a is even if and only if its last digit a0 is even.
--
-- >>> isEven 12678
-- True
--
-- >>> isEven 12671
-- False
--
isEven :: Int -> Bool
isEven = even . last . n2de

divisibleBy2 :: Int -> Bool
divisibleBy2 = isEven

-- | divisibleBy5
--
-- Thus a is divisible by 5 if and only if a0 is, which is the case precisely when a0 is 0 or 5.
--
-- >>> divisibleBy5 1205
-- True
--
-- >>> divisibleBy5 120
-- True
--
-- >>> divisibleBy5 12
-- False
--
divisibleBy5 :: Int -> Bool
divisibleBy5 n = (a0 == 0 || a0 == 5)
  where
    a0 = last $ n2de n

-- | divisibleBy3
--
-- A natural number a is divisible by 3 if and only if the sum of its digits is divisible by 3.
--
-- >>> divisibleBy3 123
-- True
--
divisibleBy3 :: Int -> Bool
divisibleBy3 = divisibleBy3or9 3

-- | divisibleBy9
--
-- A natural number a is divisible by 9 if and only if the sum of its digits is divisible by 9.
--
-- >>> divisibleBy3 1233
-- True
--
divisibleBy9 :: Int -> Bool
divisibleBy9 = divisibleBy3or9 9

divisibleBy3or9 :: Int -> Int -> Bool
divisibleBy3or9 divisor n = ((sum $ n2de n) `mod` divisor) == 0

-- | divisibleBy7
--
-- Let a be a natural number. Write a = 10b + a0, where 0 ≤ a0 < 10. Then a is divisible by 7 if and only if b − 2a0 is divisible by 7.
--
divisibleBy7 :: Int -> Bool
divisibleBy7 n = (21 * (last $ n2de n)) `mod` 7 == 0

-- | divides
--
divides :: Integral a => a -> a -> a
k `divides` n = n `div` k

-- | dividesF
--
dividesF :: Fractional a => a -> a -> a
k `dividesF` n = n / k

-- | unit
--
-- A unit in Z is a divisor of 1: -1 or +1
--
unit :: Integer
unit = 1

-- | isNonTrivial
-- non-trivial: n = kl, n /= 0, k nor l is a unit
--
isNonTrivial :: t -> Integer -> Integer -> Bool
isNonTrivial n k l = (abs k /= unit && abs l /= unit)

-- | divisionAlgorithm
--
-- (division algorithm).
-- If a and b are integers with b /= 0, then there exist unique integers q and r such that a = bq + r and 0 ≤ r < |b|
--
divisionAlgorithm :: Integral a => a -> a -> (a, a)
divisionAlgorithm a 0 = error "b /= 0"
divisionAlgorithm a b = quotRem a b

-- | divisionAlgorithmShow
--
divisionAlgorithmShow a 0 = error "b /= 0"
divisionAlgorithmShow a b = printf "(a=%d) = (b=%d)*(q=%d) + (r=%d)" a b q r
  where
    (q, r) = quotRem a b

-- | euclideanAlgorithm
--
--    Let us suppose that b /= 0. The euclidean algorithm consists of iterated application of the division algorithm to a and b until the remainder term r disappears.
--
--    a = bq1 + r1    0 <= r1 < b
--    b = r1q2 + r2   0 <= r2 < r1
--    r1 = r2q3 + r3  0 <= r3 < r2
--    ...
--    rn-2 = r(n-1)qn + rn  0 <= rn < r(n-1)
--    rn-1 = rnq(n+1)
--
-- >>> euclideanAlgorithm 13 5 :: Int
-- 2
--
euclideanAlgorithm :: Integral t => t -> t -> t
euclideanAlgorithm a b =
  case r of
       0 -> q
       _ -> euclideanAlgorithm b r
  where
    (q, r) = divisionAlgorithm a b

-- | divisors
--
-- divisors including 1
--
-- >>> divisors 15
-- [1,3,5,15]
--
-- >>> divisors 15043012
-- [1,2,4,23,46,92,113,226,452,2599,5198,10396,1447,2894,5788,33281,66562,133124,163511,327022,654044,3760753,7521506,15043012]
--
divisors :: Integer -> [Integer]
divisors n
  | n == 1    = [1]
  | otherwise = 1 : primeMultiples'products n


-- | properDivisors
--
-- divisors including 1, excluding n
--
-- >>> properDivisors 15
-- [1,3,5]
--
-- >>> properDivisors 15043012
-- [1,2,4,23,46,92,113,226,452,2599,5198,10396,1447,2894,5788,33281,66562,133124,163511,327022,654044,3760753,7521506]
--
properDivisors :: Integer -> [Integer]
properDivisors = init . divisors
