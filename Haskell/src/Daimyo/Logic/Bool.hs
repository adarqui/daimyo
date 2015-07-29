{-# LANGUAGE NoImplicitPrelude #-}

module Daimyo.Logic.Bool (
--  not,
  negation,
--  and,
  conjunction,
  (¬),
--  (&&),
  or,
  disjunction,
--  (||),
  xor,
  (<+>),
  implies,
  implication,
  (==>),
  equiv,
  equivalence,
  (<=>),
  excludedMiddle,
  valid1,
  valid2,
  valid3,
  valid4,
  logicalEquivalence1,
  logicalEquivalence2,
  logicalEquivalence3,
  contradiction1,
  contradiction2,
  contradiction3,
  contradiction4
) where

import Prelude (Bool (..), ($), (==))
import qualified Prelude as P

-- data Bool
--   = True
--   | False
--   deriving (Eq, Show)

negation :: Bool -> Bool
negation    = not

conjunction :: Bool -> Bool -> Bool
conjunction = and

disjunction :: Bool -> Bool -> Bool
disjunction = or

implication :: Bool -> Bool -> Bool
implication = implies

equivalence :: Bool -> Bool -> Bool
equivalence = equiv

(¬) :: Bool -> Bool
(¬)   = not

(&&) :: Bool -> Bool -> Bool
(&&)  = and

(||) :: Bool -> Bool -> Bool
(||)  = or

(<=>) :: Bool -> Bool -> Bool
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



-- | contradiction1
--
-- 2^1 possibilities
--
-- >>> contradiction1 (\p -> p && not p)
-- True
--
contradiction1 :: (Bool -> Bool) -> Bool
contradiction1 bf = (not $ bf True) && (not $ bf False)

-- | contradiction2
--
-- 2^2 possibilities
--
contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 bf =  (not $ bf True True)
                  && (not $ bf True False)
                  && (not $ bf False True)
                  && (not $ bf False False)

-- | contradiction3
--
-- 2^3 possibilities
--
contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 bf = P.all (==False) [ bf p q r | p <- [True, False],
                                                 q <- [True, False],
                                                 r <- [True, False]]

-- | contradiction4
--
-- 2^4 possibilities
--
contradiction4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
contradiction4 bf = P.all (==False) [ bf p q r s | p <- [True, False],
                                                   q <- [True, False],
                                                   r <- [True, False],
                                                   s <- [True, False]]

-- | Examples
--
-- >>> let (p, q) = (True, False) in (not p) `and` (p `implies` q) `equiv` not (q `and` (not p))
--
-- >>> let formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p))) in valid2 formula2
-- ?
--

-- | Laws
--
-- Law of double negation:
-- P ≡ ¬¬P
--
-- >>> logicalEquivalence1 (\p -> p) (\p -> not (not p))
-- True
--
--
-- Laws of idempotence:
-- P ∧ P ≡ P
-- P ∨ P ≡ P
--
-- >>> logicalEquivalence1 (\p -> p && p) (\p -> p)
-- True
--
-- >>> logicalEquivalence1 (\p -> p || p) (\p -> p)
-- True
--
--
-- Rewrite:
-- (P ⇒ Q) ≡ ¬P ∨ Q
-- ¬(P ⇒ Q) ≡ P ∧ ¬Q
--
-- >>> logicalEquivalence2 (\p q -> p ==> q) (\p q -> not p || q)
-- True
--
--
-- Laws of contraposition:
-- (¬P ⇒ ¬Q) ≡ (Q ⇒ P)
-- (P ⇒ ¬Q) ≡ (Q ⇒ ¬P)
-- (¬P ⇒ Q) ≡ (¬Q ⇒ P)
--
-- >>> logicalEquivalence2 (\p q -> not p ==> not q) (\p q -> q ==> p)
-- True
--
-- >>> logicalEquivalence2 (\p q -> p ==> not q) (\p q -> q ==> not p)
-- True
--
-- >>> logicalEquivalence2 (\p q -> not p ==> q) (\p q -> not q ==> p)
-- True
--
--
-- Rewrite:
-- (P ⇔ Q) ≡ ((P ⇒ Q) ∧ (Q ⇒ P))
-- (P ⇔ Q) ≡ ((P ∧ Q) ∨ (¬P ∧ ¬Q))
--
-- >>> logicalEquivalence2 (\p q -> p <=> q) (\p q -> (p ==> q) & (q ==> p))
-- True
--
-- >>> logicalEquivalence2 (\p q -> p <=> q) (\p q -> (p && q) || (not p && not q))
-- True
--
--
-- Laws of commutativity:
-- P ∧ Q ≡ Q ∧ P
-- P ∨ Q ≡ Q ∨ P
--
-- >>> logicalEquivalence2 (\p q -> p && q) (\p q -> q && p)
-- True
--
-- >>> logicalEquivalence2 (\p q -> p || q) (\p q -> q || p)
-- True
--
--
-- DeMorgan's laws:
-- ¬(P ∧ Q) ≡ ¬P ∨ ¬Q
-- ¬(P ∨ Q) ≡ ¬P ∧ ¬Q
--
-- >>> logicalEquivalence2 (\p q -> not (p && q)) (\p q -> not p || not q)
-- True
--
-- >>> logicalEquivalence2 (\p q -> not (p || q)) (\p q -> not p && not q)
-- True
--
--
-- Laws of associativity:
-- P ∧ (Q ∧ R) ≡ (P ∧ Q) ∧ R
-- P ∨ (Q ∨ R) ≡ (P ∨ Q) ∨ R
--
-- >>> logicalEquivalence3 (\p q r -> p && (q && r)) (\p q r -> (p && q) && r)
-- True
--
-- >>> logicalEquivalence3 (\p q r -> p || (q || r)) (\p q r -> (p || q) || r)
-- True
--
--
-- Distribution laws:
-- P ∧ (Q ∨ R) ≡ (P ∧ Q) ∨ (P ∧ R)
-- P ∨ (Q ∧ R) ≡ (P ∨ Q) ∧ (P ∨ R)
--
-- >>> logicalEquivalence3 (\p q r -> p && (q || r)) (\p q r -> (p && q) || (p && r))
-- True
--
-- >>> logicalEquivalence3 (\p q r -> p || (q && r)) (\p q r -> (p || q) && (p || r))
-- True
--
--
-- ¬T ≡ ⊥; ¬⊥ ≡ T
-- P ⇒ ⊥ ≡ ¬P
--
--
-- Dominance laws:
-- P ∨ T ≡ T
-- P ∧ ⊥ ≡ ⊥
--
-- Identity laws:
-- P ∨ ⊥ ≡ P
-- P ∧ T ≡ P
--
-- Law of Excluded Middle:
-- P ∨ ¬P ≡ T
--
-- Contradiction:
-- P ∧ ¬P ≡ ⊥
