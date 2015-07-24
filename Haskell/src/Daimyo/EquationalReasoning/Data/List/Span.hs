module Daimyo.EquationalReasoning.Data.List.Span (
) where

import           Prelude hiding (span)

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- > span (< 9) [1,2,3] == ([1,2,3],[])
-- > span (< 0) [1,2,3] == ([],[1,2,3])
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

-- | span_test
--
-- >>> span_test
-- True
--
span_test :: Bool
span_test = span (< 3) [1,2,3] == span_eqr

-- | span_eqr
--
-- >>> span_eqr
-- ([1,2],[3])
--
span_eqr :: [(Integer, Integer)]
span_eqr = span_eqr1 [1,2,3]
  where
    span_eqr1 (1:[2,3])
      | (< 3) 1   = let ([2],[3]) = span_eqr2 [2,3] in (1:[2],[3])
    span_eqr2 (2:[3])
      | (< 3) 2   = let ([],[3]) = span_eqr3 [3] in (2:[],[3])
    span_eqr3 (3:[])
      | otherwise = ([],[3])
