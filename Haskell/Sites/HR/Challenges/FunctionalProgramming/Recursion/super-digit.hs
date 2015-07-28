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
solution :: Integer -> Integer -> Integer
solution number replication = go initial
  where
    initial = concat $ replicate (fromIntegral replication :: Int) (digits number)
    go list
      | length list == 1 = head list
      | otherwise        = go (digits (sum list))

main :: IO ()
main = do
  [number, replication] <- fmap (map read . words) getLine
  putStrLn $ show $ solution number replication
