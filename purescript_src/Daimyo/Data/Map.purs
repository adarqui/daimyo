module Daimyo.Data.Map where

import Prelude
import Data.Array
import Data.Tuple
import qualified Data.Map as M

import Daimyo.Data.List

-- | toArray
--
-- M.toList to an array
toArray :: forall k v. M.Map k v -> Array (Tuple k v)
toArray = listToArray <<< M.toList
