main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ show $ fib (n-1)

-- | fib
--
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
