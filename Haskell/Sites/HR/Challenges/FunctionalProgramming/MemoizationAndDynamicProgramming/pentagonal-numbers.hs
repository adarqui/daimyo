-- | pentagonal
--
-- >>> pentagonal 1
-- 1
--
-- >>> pentagonal 5
-- 35
--
-- >>> pentagonal 10
-- 145
--
pentagonal :: Int -> Int
pentagonal n = (3*(n^2) - n) `div` 2

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  results <- fmap (unlines . map (show . pentagonal . read) . lines) getContents
  putStrLn results
