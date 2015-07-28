module Daimyo.Number (
  Number (..),
  isInteger,
  isWhole,
  isCounting,
  number,
  decimalPart,
  decimalPartToInteger,
  round',
  by10,
  oneBy10,
  digits,
  digitsToNumber
) where

import           Data.List
import           Data.Maybe

-- | Number
--
data Number = Number {
    n          :: Double,
    integer    :: Integer,
    fractional :: Integer,
    decimal    :: Double
} deriving (Show, Read, Eq)

-- | isInteger
--
-- >>> isInteger (5 :: Double)
-- True
--
-- >>> isInteger (-5 :: Double)
-- True
--
-- >>> isInteger (5.5 :: Double)
-- False
--
isInteger :: RealFrac a => a -> Bool
isInteger x = floor x == ceiling x

-- | isWhole
--
-- >>> isWhole (-5 :: Double)
-- False
--
-- >>> isWhole (5 :: Double)
-- True
--
isWhole :: RealFrac a => a -> Bool
isWhole x = x >= 0 && isInteger x

-- | isCounter
--
isCounting :: RealFrac a => a -> Bool
isCounting x = x > 0 && isInteger x

-- | number
--
-- >>> number 5.5
-- Number {n = 5.5, integer = 5, fractional = 5, decimal = 0.5}
--
number :: Double -> Number
number n' = Number {
  n          = n',
  integer    = truncate n',
  fractional = decimalPartToInteger n',
  decimal    = decimalPart n'
}

-- | decimalPart
--
-- >>> decimalPart 5.5
-- 0.5
--
decimalPart :: Double -> Double
decimalPart n = n - (fromIntegral (floor n) :: Double)

-- | decimalPartToInteger
--
-- >>> decimalPartToInteger 5.5 :: Int
-- 5
--
decimalPartToInteger :: Integral b => Double -> b
decimalPartToInteger n = decimalPartToInteger' (decimalPart n)
decimalPartToInteger' n
  | isInteger n = truncate n
  | otherwise   = decimalPartToInteger' (n * 10)

round' f n =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)
round'' f n =  (round $ f * (10^n)) / (10.0^^n)

-- | by10
--
-- >>> ake 5 by10
-- [10,100,1000,10000,100000]
--
by10 :: [Integer]
by10 = scanl (\acc y -> acc * 10) 10 [1..]

-- | oneBy10
--
-- >>> take 5 oneBy10
-- [1,10,100,1000,10000]
--
oneBy10 :: [Integer]
oneBy10 = 1 : by10

-- | digits
--
-- >>> digits 1234 :: [Int]
-- [1,2,3,4]
--
digits :: Integral a => a -> [a]
digits n = go n []
  where
    go 0 acc = acc
    go n acc = go (n `div` 10) (n `mod` 10 : acc)

-- | digits'
--
-- whoa this is disgusting.. atrocious
--
-- leaving this for historical value.. ABSOLUTELY INSANE!!!
--
-- >>> digits (12345 :: Int) :: [Int]
-- [1,2,3,4,5]
--
digits' :: (Integral a, Integral b) => a -> [b]
digits' n =
  map truncate $
  reverse $
  catMaybes $
  takeWhile (/= Nothing) $
  fst $
  foldl (\(list, acc) _ ->
      let
          acc' = acc / 10.0
          digit = round' (decimalPart acc') 1 * 10
      in
          if (acc == 0)
              then (list++[Nothing], 0)
              else (list++[Just digit], fromIntegral (truncate acc') :: Double)
  ) ([],fromIntegral n :: Double) $ take 100 $ repeat 10.0

-- | digitsToNumber
--
-- >>> digitsToNumber [1,2,3,4,5]
-- 12345
--
digitsToNumber :: [Integer] -> Integer
digitsToNumber ds = sum $ map (\(d,t) -> d * t) $ zip (reverse ds) oneBy10
