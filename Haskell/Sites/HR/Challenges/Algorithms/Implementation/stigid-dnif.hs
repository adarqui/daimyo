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

-- | findDigits
--
-- >>> findDigits 12
-- 2
--
-- >>> findDigits 1012
-- 3
--
findDigits :: Int -> Int
findDigits n = length $ filter (\x -> n `mod` x == 0) $ filter (/= 0) $ digits n

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  numbers <- fmap (unlines . map show . map (findDigits . read) . lines) getContents
  putStrLn numbers
