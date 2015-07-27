module Daimyo.Statistics.Examples (
  t_dogs
) where

import           Daimyo.Statistics

--
-- dogs: http://www.mathsisfun.com/data/standard-deviation.html
--

t_dogs :: IO ()
t_dogs = do
  let p = fromList [600, 470, 170, 430, 300]
  let q = fromList [601, 470, 160, 400, 350]
  putStrLn $ "population mean: " ++ (show $ mean p)
  putStrLn $ "population variance: " ++ (show $ variance p)
  putStrLn $ "population standard deviation: " ++ (show $ stddev p)
  putStrLn $ "sample mean: " ++ (show $ meanSample p)
  putStrLn $ "sample variance: " ++ (show $ varianceSample p)
  putStrLn $ "sample standard deviation: " ++ (show $ stddevSample p)
  putStrLn $ "sample z-score for 150: " ++ (show $ zscoreSample 150 p)
  putStrLn $ "sample correlation between p & q: " ++ (show $ correlationSample p q)
