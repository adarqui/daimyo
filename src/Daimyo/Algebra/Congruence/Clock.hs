module Daimyo.Algebra.Congruence.Clock (
  Clock,
  mkClock
) where

import           Control.Exception
import           Data.Ix

newtype Clock = Clock { runClock :: Integer }
  deriving (Show, Eq, Ord, Ix)

-- | Clock
--
-- >>> Clock 9 + Clock 5
-- Clock 2
--
-- >>> Clock 5 - Clock 9
-- Clock 8
--
instance Num Clock where
  Clock c1 + Clock c2 = Clock ((c1 + c2) `mod` 12)
  Clock c1 - Clock c2 = Clock ((c1 - c2) `mod` 12)
  Clock c1 * Clock c2 = Clock ((c1 * c2) `mod` 12)
  abs (Clock n) = Clock n
  signum (Clock n) = Clock (signum n)
  fromInteger n
    | n < 0     = throw Underflow
    | n > 11    = throw Overflow
    | otherwise = Clock n

-- | mkClock
--
-- >>> mkClock 13
-- Clock {runClock = 1}
--
-- >>> mkClock (-1)
-- Clock {runClock = 11}

mkClock :: Integer -> Clock
mkClock n = Clock (n `mod` 12)

instance Enum Clock where
  pred (Clock n) = mkClock (pred n)
  {-# INLINE pred #-}
  succ (Clock n) = mkClock (succ n)
  {-# INLINE succ #-}
  fromEnum (Clock n) = fromEnum n
  {-# INLINE fromEnum #-}
  toEnum n | n < 1     = error "Clock.toEnum: less than 1"
           | otherwise = Clock (toEnum n)
  {-# INLINE toEnum #-}

  enumFrom = map mkClock . enumFrom . runClock

  enumFromThen x y
    | x <= y    = map mkClock (enumFromThen   (runClock x) (runClock y))
    | otherwise = map mkClock (enumFromThenTo (runClock x) (runClock y) 0)

  enumFromTo x y = map mkClock (enumFromTo (runClock x) (runClock y))

  enumFromThenTo x x1 y
    = map Clock (enumFromThenTo (runClock x) (runClock x1) (runClock y))

instance Real Clock where
  toRational (Clock n) = toRational n

instance Integral Clock where
  toInteger = runClock
