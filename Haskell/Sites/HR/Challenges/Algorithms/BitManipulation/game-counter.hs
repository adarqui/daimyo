import           Control.Monad
import           Data.Bits     hiding (rotate)

data Player
  = Louise
  | Richard
  deriving (Show, Read, Eq, Enum)

-- | game
--
game :: Integer -> Player
game n
  | n == 1    = Richard
  | otherwise = game' Louise n

-- | game'
--
game' :: Player -> Integer -> Player
game' player 1 = rotate player
game' player n =
 if (isPowerOfTwo_ComplementAndCompare n)
    then game' (rotate player) (n - floorPowerOfTwo n)
    else game' (rotate player) (n `div` 2)

-- | rotate
--
rotate :: Player -> Player
rotate Louise  = Richard
rotate Richard = Louise

-- | ceilPowerOfTwo
--
-- >>> ceilPowerOfTwo 5
-- 8
ceilPowerOfTwo :: (Bits a, Num a) => a -> a
ceilPowerOfTwo n = 1 + foldl f (n - 1) [1,2,4,8,16,32,64]
  where f acc i = acc .|. (acc `shiftR` i)

-- | floorPowerOfTwo
--
-- >>> floorPowerOfTwo 5
-- 4
--
--  >>> floorPowerOfTwo 9
-- 8
--
floorPowerOfTwo :: (Bits a, Num a) => a -> a
floorPowerOfTwo n = (ceilPowerOfTwo n) `shiftR` 1

-- | isPowerOfTwo_ComplementAndCompare
--
-- >>> isPowerOfTwo_ComplementAndCompare 5
-- False
--
-- >>> isPowerOfTwo_ComplementAndCompare 8
-- True
--
isPowerOfTwo_ComplementAndCompare :: (Bits a, Num a) => a -> Bool
isPowerOfTwo_ComplementAndCompare x = (x /= 0) && ((x .&. (complement x + 1)) == x)

-- | main
--
main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  nums <- fmap (map read . lines) getContents
  mapM_ (putStrLn . show . game) nums
