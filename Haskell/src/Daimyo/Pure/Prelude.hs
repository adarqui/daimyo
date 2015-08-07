-- purescript-prelude: https://github.com/purescript/purescript-prelude

--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Daimyo.Pure.Prelude (
--    module A
    Unit(), unit
  , Boolean
  , (#)
  {-
  , Semigroupoid, compose, (<<<), (>>>)
  , Category, id
  , Functor, map, (<$>), (<#>), void
  , Apply, apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, bind, (>>=)
  , Monad, return, liftM1, ap
  , Semigroup, append, (<>), (++)
  , Semiring, add, zero, mul, one, (+), (*)
  , ModuloSemiring, div, mod, (/)
  , Ring, sub, negate, (-)
  , Num
  , DivisionRing
  , Eq, eq, (==), (/=)
  , Ordering(..), Ord, compare, (<), (>), (<=), (>=)
  , Bounded, top, bottom
  , BoundedOrd
  , BooleanAlgebra, conj, disj, not, (&&), (||)
  , Show, show
-}
) where

--import Control.Category as A

newtype Unit = Unit ()

unit :: Unit
unit = Unit ()

--data Boolean = True | False
type Boolean = Bool

{-
infixr 0 $
infixl 1 #

-- | $
--
($) :: forall a b. (a -> b) -> a -> b
($) f x = f x
-}

-- | #
--
(#) :: forall a b. a -> (a -> b) -> b
(#) x f = f x

{-
-- | flip
--
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- | const
--
const :: forall a b. a -> b -> a
const a _ = a

-- | asTypeOf
--
asTypeOf :: forall a. a -> a -> a
asTypeOf x _ = x

-- | otherwise
--
otherwise :: Boolean
otherwise = True

class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

-- | Semigroupoid
--
-- Laws
--
-- Associativity: p <<< (q <<< r) = (p <<< q) <<< r
--
instance Semigroupoid (->) where
  compose f g x = f (g x)

infixr 9 >>>
infixr 9 <<<

-- | (<<<)
--
(<<<) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d
(<<<) = compose

-- | (>>>)
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(>>>) = flip compose
-}

{-
-- | Category
--
-- Laws
--
-- Semigroupoid Laws
-- Identity: id <<< p = p <<< id = p
--
class (Semigroupoid a) => Category a where
  id :: forall t. a t t

instance Category (->) where
  id x = x

-- | Functor
--
-- Laws
--
-- Identity:    (<$>) id = id
-- Composition: (<$>) (f <<< g) = (f <$>) <<< (g <$>)
--
{-
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance Functor ((->) r) where
  map = compose
  -}

{-
instance Functor List where
  map = listMap
  -}

infixl 4 <$>
infixl 1 <#>

-- | <$>
--
(<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
(<$>) = map

-- | <#>
--
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
(<#>) fa f = f <$> fa

void :: forall f a. (Functor f) => f a -> f Unit
void fa = const unit <$> fa

-- | Apply
--
-- Laws
--
-- Functor Laws
-- Associative composition: (<<<) <$> f <*> g <*> h = f <*> (g <*> h)
--
class (Functor f) => Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

instance Apply ((->) r) where
  apply f g x = f x (g x)

infixl 4 <*>

-- | (<*>)
--
(<*>) :: forall f a b. (Apply f) => f (a -> b) -> f a -> f b
(<*>) = apply

-- | Applicative
--
-- Laws
--
-- Apply Laws
-- Identity:     (pure id) <*> v = v
-- Composition:  (pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)
-- Homomorphism: (pure f) <*> (pure x) = pure (f x)
-- Interchange:  u <*> (pure y) = (pure ($ y)) <*> u
--
class (Apply f) => Applicative f where
  pure :: forall a. a -> f a

instance Applicative ((->) r) where
  pure = const

-- | return
--
return :: forall m a. (Applicative m) => a -> m a
return = pure

-- | liftA1
--
liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

-- | Bind
--
-- Laws
--
-- Apply Laws
-- Associativity: (x >>= f) >>= g = x >>= (\k -> f k >>= g)
--
class (Apply m) => Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

instance Bind ((->) r) where
  bind m f x = f (m x) x

(>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
(>>=) = bind

-- | Monad
--
-- Laws
--
-- Applicative Laws
-- Bind Laws
-- Left Identity:  pure x >>= f = f x
-- Right Identity: x >>= pure = x
--
class (Applicative m, Bind m) => Monad m

instance Monad ((->) r)
--instance Monad List

-- | liftM1
--
liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
liftM1 f a =
  a >>= \a' ->
    return (f a')

-- | ap
--
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
ap f a =
  f >>= \f' ->
    a >>= \a' ->
      return (f' a')
-}
