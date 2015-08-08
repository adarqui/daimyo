--
-- reference: https://en.wikipedia.org/wiki/Xorshift
--

module Daimyo.Random.Xorshift.Xorshift64 (
  Xorshift64 (..),
  xorshift64,
  xorshifts64,
  seed64
) where

import Daimyo.Control.State
import Data.Bits

data Xorshift64
  = Xorshift64 Int
  deriving (Show, Eq, Ord)

type State64 = State Xorshift64 Int

-- | xorshift
--
-- >>> runState xorshift64 (Xorshift64 1)
-- (5180492295206395165,Xorshift64 33554433)
--
xorshift64 :: State64
xorshift64 = do
  (Xorshift64 x) <- get
  x <- return $ x `xor` x `shiftR` 12
  x <- return $ x `xor` x `shiftL` 25
  x <- return $ x `xor` x `shiftR` 27
  put $ Xorshift64 x
  return $ x * 2685821657736338717

-- | xorshifts64
--
-- >>> take 10 $ xorshifts64 (Xorshift64 1)
-- [5180492295206395165,-6066446928794000099,-5057245994778681513,5599127315341312413,1036278371763004928,5966913175426411225,3123114256791592290,7445013505872399245,1990752898006117240,-8712185931288981333]
--
-- >>> head $ drop 10000 $ xorshifts64 seed64
-- 694936809798588495
--
xorshifts64 :: Xorshift64 -> [Int]
xorshifts64 seed = go seed
  where
  go g = let (a', g') = runState xorshift64 g in a' : go g'

-- | seed64
--
seed64 :: Xorshift64
seed64 = Xorshift64 9124824867239485709
