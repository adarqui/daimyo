--
-- reference: http://www.jstatsoft.org/v08/i14/paper
--

module Daimyo.Random.Xorshift.Xorshift128 (
  Xorshift128 (..),
  xorshift128,
  xorshifts128,
  seed128
) where

import Daimyo.Control.State
import Data.Bits

data Xorshift128
  = Xorshift128 Int Int Int Int
  deriving (Show, Eq, Ord)

type State128 = State Xorshift128 Int

-- | xorshift
--
-- >>> runState xorshift128 (Xorshift128 1 2 3 4)
-- (2061,Xorshift128 2 3 4 2061)
--
xorshift128 :: State128
xorshift128 = do
  (Xorshift128 x y z w) <- get
  t <- return $ x `xor` (x `shiftL` 11)
  x <- return y
  y <- return z
  z <- return w
  w <- return $ w `xor` (w `shiftR` 19) `xor` t `xor` (t `shiftR` 8)
  put $ Xorshift128 x y z w
  return w

-- | xorshifts128
--
xorshifts128 :: Xorshift128 -> [Int]
xorshifts128 seed = fst $ foldl (\(acc, g) _ -> let (a', g') = runState xorshift128 g in (a' : acc, g')) ([], seed) [1..]

-- | seed128
--
seed128 :: Xorshift128
seed128 = Xorshift128 123456789 362436069 521288629 88675123
