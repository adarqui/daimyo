import Data.List
import Control.Monad
import Control.Applicative
import Text.Printf

fact :: Double -> Double
fact n = product [1..n]

sin :: Double -> Double
sin x = calc $ x : sc x 3

cos :: Double -> Double
cos x = calc $ 1 : sc x 2

sc :: Double -> Double -> [Double]
sc x r = [ x**n / fact n | n <- [r,r+2..r+6] ]

calc :: [Double] -> Double
calc (x:xs) = foldl' (\acc (op, n) -> acc `op` n) x $ zip (cycle [(-), (+)]) xs

rndp :: Double -> String
rndp d = printf "%.3f" d

main :: IO ()
main = do
  void $ getLine
  c <- fmap (\line -> read line :: Double) <$> liftM lines getContents
  putStrLn $ unlines $ map rndp $ concat $ map (\i -> [Main.sin i, Main.cos i]) c
