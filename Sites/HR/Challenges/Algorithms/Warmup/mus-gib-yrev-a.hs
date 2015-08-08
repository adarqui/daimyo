import           Control.Monad

main :: IO ()
main = do
  void getLine
  integers <- liftM (map (\n -> read n :: Integer) . words) getLine
  putStrLn $ (show . sum) integers
