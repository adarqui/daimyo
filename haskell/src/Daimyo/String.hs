module Daimyo.String (
  split
) where

-- | split
--
-- >>> split ',' "hey,sup"
-- ("hey","sup")
--
split :: Eq t => t -> [t] -> ([t], [t])
split c s = go [] s
  where
    go _ []       = ([], [])
    go acc (x:xs)
      | c == x    = (reverse acc, xs)
      | otherwise = go (x:acc) xs
