import Text.Printf

-- | createTriple
--
-- >>> createTriple [-4, 3, -9, 0, 4, 1]
-- (2.0,1.0,3.0)
--
createTriple :: [Double] -> (Double, Double, Double)
createTriple numbers = foldl f (0,0,0) numbers
  where
    f (neg,zero,pos) x
      | x < 0  = (neg+1,zero,pos)
      | x == 0 = (neg,zero+1,pos)
      | x > 0  = (neg,zero,pos+1)

-- | solution
--
-- >>> solution (2.0,1.0,3.0)
--
--
solution :: (Double, Double, Double) -> (Double, Double, Double)
solution (neg,zero,pos) = (neg/tot, zero/tot, pos/tot)
  where tot = neg + zero + pos

main :: IO ()
main = do
  _ <- getLine
  (neg,zero,pos) <- fmap (solution . createTriple . map read . words) getLine
  putStrLn $ printf "%.3f" pos
  putStrLn $ printf "%.3f" neg
  putStrLn $ printf "%.3f" zero
