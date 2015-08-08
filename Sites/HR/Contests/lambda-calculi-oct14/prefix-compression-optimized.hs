{-# OPTIONS -O2 #-}
import Data.List
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
 (s1:s2:[]) <- sequence [BC.getLine, BC.getLine]
 let (acc,s1',s2') = lcp s1 s2
 mapM_ (\s -> do
  putStr (show (BC.length s) ++ " ")
  BC.putStrLn s) [acc,s1',s2']

lcp m n = lcp' BC.empty m n

lcp' acc m n
 | BC.length m == 0 || BC.length n == 0 = (acc,m,n)
 | otherwise = lcp'' acc m n

lcp'' acc m n
 | BC.head m == BC.head n = lcp' (acc `BC.snoc` (BC.head m)) (BC.tail m) (BC.tail n)
 | otherwise = (acc,m,n)
