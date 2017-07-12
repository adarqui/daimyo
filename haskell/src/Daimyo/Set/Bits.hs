module Daimyo.Set.Bits (
  Set,
  emptySet,
  isEmptySet,
  containedInSet,
  addSet,
  delSet,
  toList
) where

import           Data.Bits

-- | Set
--
newtype Set
  = Set Integer
  deriving (Eq, Ord, Show)

-- | emptySet
--
emptySet :: Set
emptySet = Set $ zeroBits

-- | isEmptySet
--
isEmptySet :: Set -> Bool
isEmptySet (Set n) = n == 0

-- | containedInSet
--
containedInSet :: Int -> Set -> Bool
containedInSet i (Set set) = testBit set i

-- | addSet
--
-- >>> addSet 132 $ addSet 5 $ addSet 1 $ emptySet
-- Set 5444517870735015415413993718908291383330
--
addSet :: Int -> Set -> Set
addSet i (Set set) = Set $ setBit set i

-- | delSet
--
-- >>> delSet 132 $ addSet 132 $ addSet 5 $ addSet 1 $ emptySet
-- Set 34
--
delSet :: Int -> Set -> Set
delSet i (Set set) = Set $ clearBit set i

-- | toList
--
-- eek!
--
toList :: Num t => Set -> [t]
toList = undefined
