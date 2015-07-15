import           Control.Monad
import           Data.List

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

numTreesFromList l = ((fac (2*n))`div`((fac (n+1))*(fac n))) `mod` modp
    where
        n = toInteger $ length l

modp = ((10^8)+7)

solve n = numTreesFromList [1..n]

main :: IO ()
main = do
    t <- readLn :: IO Int
    nums <- replicateM t getLine
    let nums' = map read nums
    mapM_ (putStrLn . show . solve) nums'
