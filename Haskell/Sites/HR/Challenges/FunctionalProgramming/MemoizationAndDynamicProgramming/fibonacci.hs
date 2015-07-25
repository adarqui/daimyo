import           GHC.Arr
import           Control.Monad

-- | fibonacciNumbers
--
-- >>> take 10 $ fibonacciNumbers :: [Int]
-- [0,1,1,2,3,5,8,13,21,34]
--
fibonacciNumbers :: Integral a => [a]
fibonacciNumbers = 0 : 1 : 1 : go 1 1
  where
    go p c = (p + c) : go c (p+c)

-- | fibArray
--
-- >>> fibArray 5 :: Array Int Integer
-- array (1,5) [(1,0),(2,1),(3,1),(4,2),(5,3)]
--
fibArray :: Integral a => Int -> Array Int a
fibArray n = array (0,n) $ zip [0..n] fibs
  where
    fibs = take (n+1) $ fibonacciNumbers

-- | modp
--
modp :: Integral a => a -> a
modp n = n `mod` (10^8+7)

main :: IO ()
main = do
  _ <- getLine
  numbers <- fmap (map read . lines) getContents
  let
    max_number = maximum numbers
    memo       = fibArray max_number
  mapM_ (putStrLn . show . modp . (!) memo) numbers
