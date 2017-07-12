main :: IO ()
main = do
  n <- readLn :: IO Int
  input <- fmap lines getContents
  putStrLn $ unlines $ concat $ map (replicate n) input
