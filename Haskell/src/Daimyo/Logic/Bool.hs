{-# NoImplicitPrelude #-}

module Daimyo.Logic.Bool (
  B (..),
  not,
  negation,
  and,
  conjunction,
  (&&),
  or,
  disjunction,
  (||),
  xor,
  implies,
  implication,
  (==>),
  equiv,
  (<=>)
) where

import Prelude (Eq, Show)

data B
  = T
  | F
  deriving (Eq, Show)

negation    = not
conjunction = and
disjunction = or
implication = implies
equivalence = equiv

(&&)  = and
(||)  = or
(<=>) = equiv

-- | not
--
-- P | ¬P
-- T | F
-- F | T
--
not :: B -> B
not F = T
not T = F

-- | and
--
-- P Q | P ∧ Q
-- T T |   T
-- T F |   F
-- F T |   F
-- F F |   F
--
and :: B -> B -> B
and T T = T
and T F = F
and F T = F
and F F = F

-- | or
--
-- P Q | P ∨ Q
-- T T |   T
-- T F |   T
-- F T |   T
-- F F |   F
--
or :: B -> B -> B
or T T = T
or T F = T
or F T = T
or F F = F

-- | xor
--
-- P Q | P ∨ Q
-- T T |   F
-- T F |   T
-- F T |   T
-- F F |   F
--
xor :: B -> B -> B
xor T T = F
xor T F = T
xor F T = T
xor F F = F

-- | <+> is the same as exclusive or
--
(<+>) :: B -> B -> B
x <+> y = x /= y

-- | /=
--
(/=) :: B -> B -> B
x /= y = not (x `equiv` y)

-- | implies
--
-- P Q | P => Q
-- T T |   T
-- T F |   F
-- F T |   T
-- F F |   T
implies :: B -> B -> B
implies T T = T
implies T F = F
implies F T = T
implies F F = T

-- | another implication
--
infix 1 ==>
(==>) :: B -> B -> B
x ==> y = not x `or` y

-- | equivalence
--
-- P Q | P <=> Q
-- T T |    T
-- T F |    F
-- F T |    F
-- F F |    T
--
equiv :: B -> B -> B
equiv T T = T
equiv T F = F
equiv F T = F
equiv F F = T

-- | Examples
--
-- >>> let (p, q) = (T, F) in (not p) `and` (p `implies` q) `equiv` not (q `and` (not p))
--
--
