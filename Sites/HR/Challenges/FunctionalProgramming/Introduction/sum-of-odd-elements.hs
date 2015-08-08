main :: IO ()
main = do
  s <- fmap (show . sum . filter odd . map read . lines) getContents
  putStrLn s
