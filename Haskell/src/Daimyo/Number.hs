module Daimyo.Number (
    isInteger,
    isWhole,
    isCounting
) where

isInteger x = floor x == ceiling x

isWhole x = x >= 0 && isInteger x

isCounting x = x > 0 && isInteger x
