-- this is so "dumb".. but they wanted an array.

import GHC.Arr
import Control.Applicative
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  contents <- lines <$> getContents
  putStrLn $ show $ solution n contents

-- | solution
--
-- >>> solution 3 ["10","9","8","2","7","5","1","3","0"]
--
solution :: Int -> [String] -> String
solution n xs = unlines $ map show $ solution' n (map read xs)

-- | solution'
--
-- >>>
--
solution' :: Int -> [Int] -> [Int]
solution' n numbers = catMaybes $ elems $ filterArray f arr
  where
    arr = newArray 100 numbers
    f Nothing = Nothing
    f (Just x)
      | x <= n = Just x
      | otherwise = Nothing

-- | newArray
--
newArray :: Int -> [Int] -> Array Int (Maybe Int)
newArray sz numbers = array (0, sz) mapped
  where
    zero = zip [0..sz] (repeat Nothing)
    mapped = zero ++ map (\x -> (x, Just x)) numbers

-- | filterArray
--
filterArray :: (Maybe Int -> Maybe Int) -> Array Int (Maybe Int) -> Array Int (Maybe Int)
filterArray f arr = amap f arr
