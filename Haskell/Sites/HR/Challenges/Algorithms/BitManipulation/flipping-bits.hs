import           Data.Bits

-- | flipBits
--
-- >>> 2147483647
-- 2147483648
--
-- >>> 1
-- 4294967294
--
-- >>> 0
-- 4294967295
--
flipBits :: Int -> Int
flipBits n = foldl complementBit n [0..31]

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  numbers <- fmap (unlines . map (show . flipBits . read) . lines) getContents
  putStrLn numbers
