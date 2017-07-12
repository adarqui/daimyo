--
-- reference: http://www.jstatsoft.org/v08/i14/paper
--

module Daimyo.Random.Xorshift.Xorshift128 (
  Xorshift128 (..),
  xorshift128,
  xorshifts128,
  seed128
) where

import           Daimyo.Control.State
import           Data.Bits

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
-- >>> take 10 $ xorshifts128 seed128
-- [252977563114,646616338854,476657867818,294684809458,517442357777055,1520438133548453,1708732760301528,1183697501400772,1062766255963462739,2713212276630031108]
--
-- >>> head $ drop 10000 $ xorshifts128 seed128
-- 1767023184024352365
--
xorshifts128 :: Xorshift128 -> [Int]
xorshifts128 seed = go seed
  where
  go g = let (a', g') = runState xorshift128 g in a' : go g'

-- | seed128
--
seed128 :: Xorshift128
seed128 = Xorshift128 123456789 362436069 521288629 88675123
