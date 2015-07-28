-- | mingle
--
-- >>> mingle "abcde" "pqrst"
-- "apbqcrdset"
--
mingle :: String -> String -> String
mingle []     _      = []
mingle (x:xs) (y:ys) = x : y : mingle xs ys

main :: IO ()
main = do
  [xs, ys] <- fmap lines getContents
  putStrLn $ mingle xs ys
