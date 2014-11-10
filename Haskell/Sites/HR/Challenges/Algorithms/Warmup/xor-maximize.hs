import Data.Bits
import Data.List

main :: IO ()
main = do
    _L <- readLn :: IO Int
    _R <- readLn :: IO Int
    putStrLn $ show $ max'xor $ pairs _L _R

pairs l r = [ (i,j) | i <- [l..r], j <- [l..r] ]
max'xor nums = foldl' (\acc (i,j) -> max (i `xor` j) acc) 0 nums
