{-# LANGUAGE NoImplicitPrelude #-}

module Daimyo.Logic.Bool (
  not,
  negation,
  and,
  conjunction,
  (&&),
  or,
  disjunction,
  (||),
  xor,
  (<+>),
  implies,
  implication,
  (==>),
  equiv,
  (<=>),
  excludedMiddle,
  valid1,
  valid2,
  valid3,
  valid4,
  logicalEquivalence1,
  logicalEquivalence2,
  logicalEquivalence3
) where

import Prelude (Eq, Show, Bool (..))
import qualified Prelude as P

-- data Bool
--   = True
--   | False
--   deriving (Eq, Show)

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
-- True | False
-- False | True
--
not :: Bool -> Bool
not False = True
not True  = False

-- | and
--
-- P Q | P ∧ Q
-- True True |   True
-- True False |   False
-- False True |   False
-- False False |   False
--
and :: Bool -> Bool -> Bool
and True True = True
and True False = False
and False True = False
and False False = False

-- | or
--
-- P Q | P ∨ Q
-- True True |   True
-- True False |   True
-- False True |   True
-- False False |   False
--
or :: Bool -> Bool -> Bool
or True True   = True
or True False  = True
or False True  = True
or False False = False

-- | xor
--
-- P Q | P ∨ Q
-- True True |   False
-- True False |   True
-- False True |   True
-- False False |   False
--
-- >>> True == True `xor` False `xor` False
-- True
--
-- >>> True == True `xor` True `xor` True
-- True
--
xor :: Bool -> Bool -> Bool
xor True True   = False
xor True False  = True
xor False True  = True
xor False False = False

-- | <+> is the same as exclusive or
--
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- | /=
--
(/=) :: Bool -> Bool -> Bool
x /= y = not (x `equiv` y)

-- | implies
--
-- P Q | P => Q
-- True True |   True
-- True False |   False
-- False True |   True
-- False False |   True
implies :: Bool -> Bool -> Bool
implies True True   = True
implies True False  = False
implies False True  = True
implies False False = True

-- | another implication
--
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = not x `or` y

-- | equivalence
--
-- P Q | P <=> Q
-- True True |    True
-- True False |    False
-- False True |    False
-- False False |    True
--
equiv :: Bool -> Bool -> Bool
equiv True True   = True
equiv True False  = False
equiv False True  = False
equiv False False = True

-- | excludedMiddle
--
-- >>> excludedMiddle True False
-- True
--
-- >>> excludedMiddle False False
-- True
--
excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

-- | valid1
--
-- 2^1 possibilities
--
-- >>> valid1 excludedMiddle
-- True
--
valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

-- | valid2
--
-- 2^2 possibilities
--
-- >>> valid2 (\p q -> p ==> (q ==> p))
-- True
--
-- >>> valid2 (\p q -> (p ==> q) ==> p)
-- False
--
-- >>> valid2 (\p q -> p <=> ((p <+> q) <+> q))
-- True
--
valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =  (bf True True)
          && (bf True False)
          && (bf False True)
          && (bf False False)

-- | valid3
--
-- 2^3 possibilities
--
valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = P.and [ bf p q r | p <- [True, False],
                               q <- [True, False],
                               r <- [True, False]]

-- | valid4
--
-- 2^4 possibilities
--
valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = P.and [ bf p q r s | p <- [True, False],
                                 q <- [True, False],
                                 r <- [True, False],
                                 s <- [True, False]]

-- | logicalEquivalence1
--
logicalEquivalence1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logicalEquivalence1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

-- | logicalEquivalence2
--
-- >>> logicalEquivalence2 const (\p q -> (p <+> q) <+> q)
-- True
--
logicalEquivalence2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logicalEquivalence2 bf1 bf2 = P.and [bf1 p q <=> bf2 p q | p <- [True, False],
                                                          q <- [True, False]]

-- | logicalEquivalence3
--
logicalEquivalence3 ::
  (Bool -> Bool -> Bool -> Bool) ->
  (Bool -> Bool -> Bool -> Bool) ->
  Bool
logicalEquivalence3 bf1 bf2 = P.and [bf1 p q r <=> bf2 p q r | p <- [True, False],
                                                              q <- [True, False],
                                                              r <- [True, False]]
-- | Examples
--
-- >>> let (p, q) = (True, False) in (not p) `and` (p `implies` q) `equiv` not (q `and` (not p))
--
-- >>> let formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p))) in valid2 formula2
-- ?
--
