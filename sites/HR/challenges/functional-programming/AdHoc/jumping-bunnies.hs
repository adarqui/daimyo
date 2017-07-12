import           Data.List

main :: IO ()
main = do
  _ <- getLine
  meeting_spot <- fmap (show . foldl' lcm 1 . map read . words) getLine
  putStrLn meeting_spot
