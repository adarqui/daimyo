-- | permute
--
-- >>> permute "abcdpqrs"
-- "badcqpsr"
--
permute :: String -> String
permute []       = []
permute (x:y:ys) = y : x : permute ys

main :: IO ()
main = do
  _ <- getLine
  output <- fmap (unlines . map permute . lines) getContents
  putStrLn output
