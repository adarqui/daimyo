{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Text.Printf

main :: IO ()
main = do
  [x, y] :: [Int] <- (map read . words) <$> getLine
  putStrLn $ show $ myGcd x y

-- | myGcd
--
-- >>> myGcd 10 100
-- 10
--
myGcd :: Integral a => a -> a -> a
myGcd a b
      | a == b = a
      | a > b  = gcd (a-b) b
      | b > a  = gcd a (b-a)

-- | format
--
-- >>> format 10 100 10
-- "GCD(10,100) = 10"
--
format :: Int -> Int -> Int -> String
format = printf "GCD(%d,%d) = %d"
