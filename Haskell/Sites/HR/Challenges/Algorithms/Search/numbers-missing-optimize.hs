{-# OPTIONS -O2 #-}
import           Data.List
import qualified Data.Map  as M

main :: IO ()
main = do
 n <- readLn :: IO Int
 n' <- getLine
 m <- readLn :: IO Int
 m' <- getLine
 case (1 <= n && m <= 1000010) of
  True -> do
   let n'ints = count'List $ build'List n'
   let m'ints = count'List $ build'List m'
   putStrLn $ prettify'result $ missing'List n'ints m'ints
  _ -> error "Constraint Error: 1 <= n && m <= 100010"

build'List s = read ("[" ++ (map (\n -> if n == ' ' then ',' else n) s) ++ "]") :: [Int]
count'List xs = M.fromListWith (\x y -> y + 1) $ map (\z -> (z, 1)) xs
missing'List m1 m2 = map (\(x,y) -> x) $ M.toAscList $ M.filter (/= 0) $ M.unionWith (-) m1 m2
prettify'result xs = concat $ intersperse " " $ map show xs
