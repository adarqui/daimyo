{-
 - source: https://www.hackerrank.com/challenges/counter-game
 -}

import           Control.Monad
import           Data.Bits     hiding (rotate)

data Player = Louise | Richard deriving (Show, Read, Eq, Enum)

main :: IO ()
main = do
 _T <- readLn :: IO Int
 nums <- mapM (\_ -> readLn :: IO Integer) [1.._T]
 mapM_ (\n -> putStrLn $ show $ game n) nums

game :: Integer -> Player
game n
  | n == 1 = Richard
  | otherwise = game' Louise n

game' :: Player -> Integer -> Player
game' player 1 = rotate player
game' player n =
 case (isPowerOfTwo'ComplementAndCompare n) of
  False -> game' rot (n - floorPowerOfTwo n)
  True -> game' rot (n `div` 2)
 where
  rot = rotate player

rotate :: Player -> Player
rotate Louise = Richard
rotate Richard = Louise

ceilPowerOfTwo n = 1 + foldl (\acc i -> acc .|. (acc `shiftR` i)) (n - 1) [1,2,4,8,16,32,64]

floorPowerOfTwo n = (ceilPowerOfTwo n) `shiftR` 1

isPowerOfTwo'ComplementAndCompare x = (x /= 0) && ((x .&. (complement x + 1)) == x)
