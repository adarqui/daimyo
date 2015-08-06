module Pure.Data.Map where

import Prelude
import Data.Array
import Data.Tuple
import qualified Data.Map as M

import Pure.Data.List

-- | toArray
--
-- M.toList to an array
toArray :: forall k v. M.Map k v -> Array (Tuple k v)
toArray = listToArray <<< M.toList
