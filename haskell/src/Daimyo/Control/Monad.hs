module Daimyo.Control.Monad (
  repeatM,
  repeatM_
) where

import           Control.Monad ()

repeatM :: Monad m => m a -> m [a]
repeatM f = sequence (repeat f)

repeatM_ :: Monad m => m a -> m ()
repeatM_ f = sequence_ (repeat f)
