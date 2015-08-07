-- purescript-bifunctor
--

{-# LANGUAGE RankNTypes #-}

module Daimyo.Pure.Data.Bifunctor (
  Bifunctor (..),
  lmap,
  rmap
) where

import           Control.Category
import           Prelude          hiding (id)

-- | Bifunctor
--
-- Laws
--
-- Identity:    bimap id id == id
-- Composition: bimap f1 g1 <<< bimap f2 g2 == bimap (f1 <<< f2) (g1 <<< g2)
--
class Bifunctor f where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

-- | lmap
--
lmap :: forall f a b c. (Bifunctor f) => (a -> b) -> f a c -> f b c
lmap f = bimap f id

-- | rmap
--
rmap :: forall f a b c. (Bifunctor f) => (b -> c) -> f a b -> f a c
rmap = bimap id

-- | (,)
--
-- >>> bimap (+1) (+2) (0,0) :: (Int,Int)
-- (1,2)
instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)
