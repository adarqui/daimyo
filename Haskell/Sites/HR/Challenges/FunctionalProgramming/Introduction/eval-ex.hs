import Control.Monad
import Data.List
import Text.Printf

fac 0 = 1
fac n = n * fac (n-1)

rounder i n = (fromInteger $ round $ i * (10^n)) / (10.0^^n)

-- 1 + x + x2/2! + x3/3! + x4/4! + .......
solve :: Float -> Float
solve x = 1 + x + foldl' (\acc i -> acc + (x**i/fac i)) 0 [2..9]

main :: IO ()
main = do
    t <- readLn :: IO Int
    values <- replicateM t getLine
    let values' = map (\v -> read v :: Float) values
    mapM_ (\n -> printf "%.4f\n" $ solve n) values'
