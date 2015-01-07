module Daimyo.Number (
    Number (..),
    isInteger,
    isWhole,
    isCounting,
    number,
    decimalPart,
    decimalPart'to'Integer,
    round',
    by10,
    one'by10,
    digits,
    digits'to'num
) where

import Data.List
import Data.Maybe

data Number = Number {
    n :: Double,
    integer :: Integer,
    fractional :: Integer,
    decimal :: Double
} deriving (Show, Read, Eq)

isInteger x = floor x == ceiling x

isWhole x = x >= 0 && isInteger x

isCounting x = x > 0 && isInteger x

{- precision life.. -}

number :: Double -> Number
number n' = Number {
    n = n',
    integer = truncate n',
    fractional = decimalPart'to'Integer n',
    decimal = decimalPart n'
}

decimalPart n = n - (fromIntegral (floor n) :: Double)

decimalPart'to'Integer n = decimalPart'to'Integer' (decimalPart n)
decimalPart'to'Integer' n
    | isInteger n = truncate n
    | otherwise = decimalPart'to'Integer' (n * 10)

round' f n =  (fromInteger $ round $ f * (10^n)) / (10.0^^n)
round'' f n =  (round $ f * (10^n)) / (10.0^^n)

by10 = scanl (\acc y -> acc * 10) 10 [1..]
one'by10 = 1 : by10

{-
    extracts the digits from an integer
-}

digits n =
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


digits'to'num ds = sum $ map (\(d,t) -> d * t) $ zip (reverse ds) one'by10

t_digits = digits 1992132213246009
