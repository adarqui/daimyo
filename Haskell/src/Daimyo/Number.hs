module Daimyo.Number (
    Number (..),
    isInteger,
    isWhole,
    isCounting,
    number,
    decimalPart,
    decimalPart'to'Integer
) where

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
