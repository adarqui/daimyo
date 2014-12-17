module Daimyo.Algebra.Congruence.Clock (
    Clock
) where

newtype Clock = Clock Int deriving (Show, Eq)

instance Num Clock where
    (Clock c1) + (Clock c2) = Clock ((c1 + c2) `mod` 12)
    (Clock c1) - (Clock c2) = Clock ((c1 - c2) `mod` 12)

clock'add :: Clock -> Clock -> Clock
clock'add a b = a + b

clock'sub :: Clock -> Clock -> Clock
clock'sub a b = a - b

t_clock'add'1 = clock'add (Clock 9) (Clock 5)
t_clock'sub'1 = clock'sub (Clock 5) (Clock 9)
