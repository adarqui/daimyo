import Control.Applicative

main :: IO ()
main = do
  n <- readLn :: IO Int
  contents <- lines <$> getContents
  putStrLn $ solution n contents

-- | solution
--
-- >>> solution 3 ["10","9","8","2","7","5","1","3","0"]
--
solution :: Int -> [String] -> String
solution n xs = unlines $ map show $ filter (< n) $ map read xs
