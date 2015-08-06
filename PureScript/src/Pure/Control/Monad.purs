module Pure.Control.Monad where

import Prelude
import Data.Array
import Data.Maybe
import Data.Tuple

mapM :: forall eff m a b. (Monad m, Applicative m) => (a -> m b) -> Array a -> m (Array b)
mapM f xs = do
  let
    h = head xs
    t = tail xs
  case Tuple h t of
       Tuple Nothing _ -> return []
       Tuple (Just h') (Just t') -> do
         y  <- f h'
         ys <- mapM f t'
         return (y : ys)
