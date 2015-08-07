-- purescript-profunctor
--

{-# LANGUAGE RankNTypes #-}

module Daimyo.Pure.Data.Profunctor (
  Profunctor (..),
  lmap,
  rmap,
  arr
) where

import           Control.Category
import           Daimyo.Pure.Prelude
import           Prelude             hiding (id)

-- | Profunctor
--
-- Laws
--
-- Identity: dimap id id = id
-- Composition: dimap f1 g1 <<< dimap f2 g2 = dimap (f1 >>> f2) (g1 <<< g2)
--
class Profunctor p where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

-- | lmap
--
lmap :: forall a b c p. (Profunctor p) => (a -> b) -> p b c -> p a c
lmap a2b = dimap a2b id

-- | rmap
--
rmap :: forall a b c p. (Profunctor p) => (b -> c) -> p a b -> p a c
rmap b2c = dimap id b2c

-- | arr
--
arr :: forall a b p. (Category p, Profunctor p) => (a -> b) -> p a b
arr f = rmap f id

-- | ->
--
-- >>> dimap (+1) (+2) (+3) 0 :: Int
-- 6
--
-- >>> lmap (*2) (+2) 0 :: Int
-- 2
--
-- >>> rmap (*2) (+2) 0 :: Int
-- 4
--
-- >>> arr (+1) 1 :: Int
-- 2
--
instance Profunctor (->) where
  dimap a2b c2d b2c = a2b >>> b2c >>> c2d
