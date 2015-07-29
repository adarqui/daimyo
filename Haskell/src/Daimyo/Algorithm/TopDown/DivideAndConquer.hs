module Daimyo.Algorithm.TopDown.DivideAndConquer (
  divideAndConquer
) where

type Problem p   = p
type Solution s  = s
type Ind p       = Problem p -> Bool
type Solve p s   = Problem p -> Solution s
type Divide p    = Problem p -> Problem [p]
type Combine p s = Problem p -> Solution [s] -> Solution s

-- | ind
--
-- returns True if an instance of the problem is indivisible and False otherwise
--
-- ind :: p -> Bool

-- | solve
--
-- solves an indivisible problem instance
--
-- solve :: p -> s

-- | divide
--
-- divides a problem into a list of subproblems
--
-- divide :: p -> [p]

-- | combine
--
-- given the original problem and solution of its subproblems, this function combines them into a single solution
--
-- combine :: p -> [s] -> s

-- | divideAndConquer
--
-- mostly taken from AAFA
--
divideAndConquer :: Ind p -> Solve p s -> Divide p -> Combine p s -> Problem p -> Solution s
divideAndConquer ind' solve' divide' combine' initial = go initial
  where
    go pb
      | ind' pb   = solve' pb
      | otherwise = combine' pb (map go (divide' pb))
