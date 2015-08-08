module Daimyo.Data.List where

import Prelude
import Data.Array (reverse, (:))
import Data.List hiding (reverse, (:))

-- | listToArray
--
-- convert a list to an array: not sure if this is efficient yet
--
listToArray :: forall a. List a -> Array a
listToArray = go []
  where
  go acc Nil         = reverse acc
  go acc (Cons x xs) = go (x : acc) xs
