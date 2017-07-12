import Data.List

-- | rotate
--
-- >>> rotate "abc"
-- ["bca","cab","abc"]
--
-- rotate "abcde"
-- ["bcdea","cdeab","deabc","eabcd","abcde"]
--
rotate :: String -> [String]
rotate s = go (length s) s
  where
    go 0 _ = []
    go n (x:xs) = s' : go (n-1) s'
      where
        s' = (xs++[x])

-- | solution
--
-- >>> solution ["abc", "aaa", "z"]
-- "bca cab abc\naaa aaa aaa\nz\n"
--
solution :: [String] -> String
solution contents = unlines $ map (concat . intersperse " " . rotate) contents

main :: IO ()
main = do
  _ <- getLine
  contents <- fmap lines getContents
  putStrLn $ solution contents
