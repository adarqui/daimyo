module Daimyo.String (
  split
) where

-- | split
--
-- >>> split ',' "hey,sup"
-- ("hey","sup")
--
split :: Eq t => t -> [t] -> ([t], [t])
split c s = go [] c s
  where
    go _ _ []     = ([], [])
    go acc c (x:xs)
      | c == x    = (reverse acc, xs)
      | otherwise = go (x:acc) c xs
