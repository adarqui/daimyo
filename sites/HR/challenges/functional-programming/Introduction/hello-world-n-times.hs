import           Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  replicateM_ n (putStrLn "Hello World")
