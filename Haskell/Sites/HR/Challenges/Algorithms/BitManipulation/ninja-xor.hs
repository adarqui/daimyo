{-# OPTIONS -O2 #-}
import Data.List
import Data.Bits
import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 pairs <- mapM (\_ -> getPair) [1.._T]
 mapM_ (\(n, nums) -> putStrLn $ show $ solve nums) pairs

solve nums = (foldl (\acc l -> acc + xorsum l) 0 $ subsequences nums) `mod` modp

modp = ((10^9) + 7)

xorsum [] = 0
xorsum (x:xs) = x `xor` xorsum xs

getPair :: IO (Int,[Int])
getPair = do
 n <- readLn :: IO Int
 nums <- getLine >>= \line -> return $ map (\n -> read n :: Int) $ words line
 return (n, nums)

subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]
