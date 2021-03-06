import Data.List

-- | digits
--
-- >>> digits 1234 :: [Integer]
-- [1,2,3,4]
--
digits :: Integral a => a -> [a]
digits = go []
  where
    go acc 0 = acc
    go acc n = go (n `mod` 10 : acc) (n `div` 10)

-- | smartDigits
--
-- >>> smartDigits 148 3
-- [3,9]
--
-- >>> smartDigits 9875 1
-- [2,9]
--
smartDigits :: Integer -> Integer -> [Integer]
smartDigits number replication = dropZeros $ digits (sum' * replication)
  where
    sum' = foldl' (+) 0 $ digits number

-- | dropZeros
--
-- >>> dropZeros [6,0,0,0]
-- [6]
--
dropZeros :: [Integer] -> [Integer]
dropZeros = filter (/=0)

-- | sanitize
--
sanitize :: [Integer] -> [Integer]
sanitize xs
  | elem 9 xs = 9 : filter (/= 9) xs
  | otherwise = xs

-- | solution
--
-- >>> solution 148 3
-- 3
--
-- >>> solution 9875 1
-- 2
--
-- >>> solution 148 100
-- 4
--
solution' :: Integer -> Integer -> Integer
solution' number replication = go $ digits (replication * go initial)
  where
    initial = sanitize (smartDigits number 1)
    go []                = 0
    go list
      | length list == 1 = head list
      | otherwise        = go (digits (sum list))

-- | something keeps timing out, feh
--
solution :: Integer -> Integer -> Integer
solution number replication =
  if answer == 0
     then 9
     else answer
  where
    answer = (number * replication) `mod` 9

main :: IO ()
main = do
  [number, replication] <- fmap (map read . words) getLine
  putStrLn $ show $ solution number replication
