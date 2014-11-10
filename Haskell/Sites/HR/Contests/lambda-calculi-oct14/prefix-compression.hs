import Data.List

main :: IO ()
main = do
 (s1:s2:[]) <- sequence [getLine, getLine]
 let (acc,s1',s2') = lcp s1 s2
 mapM_ (\s -> putStrLn $ (show $ length s) ++ " " ++ s) [acc,s1',s2']

lcp m n = lcp' [] m n

lcp' acc (m:ms) (n:ns)
 | m == n = lcp' (acc ++ [m]) ms ns
lcp' acc m n = (acc,m,n)

