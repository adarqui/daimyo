import Control.Monad
import Data.Maybe

main :: IO ()
main = do
 _T <- readLn 
 case (1 <= _T && _T <= 15) of
  True -> do
   m <- mapM (\x -> getLine >>= \line -> return $ parse line) [1.._T]
   mapM_ (\(x,y) -> putStrLn $ show x ++ " " ++ show y) $ map (\(p,q) -> symmetry p q) m
  _ -> error "Constraint Error: 1 <= T <= 15"

parse line = ((px,py),(qx,qy))
 where
  nums = words line
  px = read (nums !! 0) :: Int
  py = read (nums !! 1) :: Int
  qx = read (nums !! 2) :: Int
  qy = read (nums !! 3) :: Int

symmetry (px,py) (qx,qy) = (2 * qx - px, 2 * qy - py)
