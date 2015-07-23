main :: IO ()
main = do
  s <- fmap (unlines . map snd . filter (\(x,y) -> odd x) . zip [0..] . lines) getContents
  putStrLn s
