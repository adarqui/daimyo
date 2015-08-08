import Prelude
import Data.Array hiding (reverse, (:))
import Data.List (List (..), reverse, (:))
import Data.Foldable

-- | arrayToList
--
-- convert an array to a list
--
arrayToList :: forall a. Array a -> List a
arrayToList xs = reverse $ foldl (\acc x -> (x : acc)) Nil xs
