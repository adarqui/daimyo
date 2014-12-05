{-# LANGUAGE ParallelListComp #-}

{-
    sources.

    book: http://math.boisestate.edu/~holmes/holmes/head.pdf
    symbols: http://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
-} 

module Holmes (
) where

{- todo
    <- is
    | such that
-}

import Data.Set
import Data.Maybe
import qualified Data.List as L

data ℕ = Z | Succ ℕ 

toSet :: (Ord a) => [a] -> Set a
toSet = fromList

fromSet :: Set a -> [a]
fromSet = toList

elementOf :: (Ord a) => a -> Set a -> Bool
elementOf = member

notElementOf :: (Ord a) => a -> Set a -> Bool
notElementOf x = not . elementOf x

-- (∀) ..
-- ∃
-- ∅
-- ∈
-- ∉
-- ∋
-- ∌
-- ∑
-- ∆
-- ⨁

(∈) :: Ord a => a -> Set a -> Bool
(∈) = elementOf

(∉) :: Ord a => a -> Set a -> Bool
(∉) = notElementOf

--infixl 9 ∪
(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = axiom_of_union

--infixl 9 ∩
(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = theorem_intersection

(⊆) :: Ord a => Set a -> Set a -> Bool
(⊆) = def_subset

(⊂) :: Ord a => Set a -> Set a -> Bool
(⊂) = def_proper_subset

(⊇) :: Ord a => Set a -> Set a -> Bool
(⊇) = def_superset

(⊃) :: Ord a => Set a -> Set a -> Bool
(⊃) = def_proper_superset

--infixl 9 .=.
(.=.) :: Ord a => Set a -> Set a -> Bool
(.=.) = axiom_of_extensionality

interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) ys = x : interleave ys xs

oddNats = [1,3..]
evenNats = [0,2..]
nats = interleave evenNats oddNats

emptyBool = empty :: Set Bool
emptyNat = empty :: Set Int

t_setNat n = toSet [0..n]
t_setPosNat n = toSet [1..n]

{-
    ***
    Chapter 2: The Set Concept
    ***
-}


{-
    Axiom of Extensionality.

    If A and B are sets, and for each x, x is an element of A if and only if x is an element of B, then A = B.

    If A and B are sets, and for each x, x is an element of A if and only if x `elementOf` of B, then A `axiom_of_extensionality` B.
-}

axiom_of_extensionality :: (Eq a, Ord a) => Set a -> Set a -> Bool
axiom_of_extensionality _A _B
    | size _A == size _B =
        let _A'elements = elems _A in
        let _B'elements = elems _B in
        and [ member b _A && member a _B | a <- _A'elements , b <- _B'elements]
    | otherwise = False


axiom_of_extensionality' :: (Eq a, Ord a) => Set a -> Set a -> Bool
axiom_of_extensionality' _A _B =
    elems _A == elems _B


t_proof_axiom_of_extensionality =
    let results = [t1, t2, t3, t4, t5, t6, t7] in
    (and results, results)
    where
        t1 = axiom_of_extensionality emptyBool emptyBool
        t2 = not $ axiom_of_extensionality emptyNat (t_setNat 1)
        t3 = not $ axiom_of_extensionality (t_setNat 1) emptyNat
        t4 = not $ axiom_of_extensionality (t_setNat 10) (t_setNat 11)
        t5 = not $ axiom_of_extensionality (t_setPosNat 10) (t_setNat 10)
        t6 = axiom_of_extensionality (t_setNat 10) (t_setNat 10)
        t7 = axiom_of_extensionality (t_setPosNat 10) (t_setPosNat 10)


{-
    Axiom of Atoms.

     If x is an atom, then for all y, y notElemOf x
-}


{-
    Theorem.

    If A and B are sets, and for all x, x is not an element of A and x is not an element of B, then A = B
-}

-- theorem_1 _A _B =



{-
    Chapter 3: Boolean Operations on Sets
-}

data Sets = Empty | Universe
--data Sets' = ∅ | V

universe x = [ x | x <- x ]
universe' x = [ x | x <- x, True ]
emptyset x = [ x | x <- x, x /= x ]
emptyset' x = [ x | x <- x, False ]
-- emptyset'' = complement V


{-
    Axiom of the Universal Set.

    { x | x = x }, also called V, exists.
-}


{-
    Axiom of Complements.

    For each set A, the set A^c = {x | x notElem A}, called the complement of A, exists.
-}


{-
    Axiom of (Boolean) Unions.

    If A and B are set, the set A union B = {x | x elemOf A or x elemOf B or both}, called the (Boolean) union of A and B, exists.
-}

axiom_of_union :: Ord a => Set a -> Set a -> Set a
axiom_of_union _A _B =
    let _A'elements = elems _A in
    let _B'elements = elems _B in
    toSet $ _A'elements ++ _B'elements
    

{-
    Theorem.

    For each set A and B, the set A intersection B = {x | x elemOf A and x elemOf B}, called the (Boolean) intersection of A and B, exists.
-}


theorem_intersection :: Ord a => Set a -> Set a -> Set a
theorem_intersection _A _B =
    let _A'elements = elems _A in
    let _B'elements = elems _B in
    toSet $ [ x | x <- _A'elements, x `elementOf` _B] ++ [ x | x <- _B'elements, x `elementOf` _A ]


theorem_intersection' :: Ord a => Set a -> Set a -> Set a
theorem_intersection' _A _B =
    let _A'elements = elems _A in
    let _B'elements = elems _B in
    toSet $ [ x | x <- _A'elements, x ∈ _B] ++ [ x | x <- _B'elements, x ∈ _A ]

{-
    Theorem.

    For each pair of sets A, B, the set

    B - A = {x | x elementOf B and x notElemOf A}

    called the relative complemenet of A with respect to B, exists
-}

theorem_relative_complement :: Ord a => Set a -> Set a -> Set a
theorem_relative_complement _B _A =
    let _B'elements = elems _B in
    toSet [ x | x <- _B'elements, x `notElementOf` _A ]

theorem_relative_complement' :: Ord a => Set a -> Set a -> Set a
theorem_relative_complement' _B _A =
    let _B'elements = elems _B in
    toSet [ x | x <- _B'elements, x ∉ _A ]

{-
    Definition.

    For A, B sets, we define the symmetric difference A ∆ B as (B - A) ∪ (A − B)
    For A, B sets, we define the symmetric difference A `def_symmetric_difference` B as (B `theorem_relative_complement` A) `axiom_of_union` (A `theorem_relative_complement` B)
-}

def_symmetric_difference :: (Ord a) => Set a -> Set a -> Set a
def_symmetric_difference _A _B =
    let _BArc = theorem_relative_complement _B _A in
    let _ABrc = theorem_relative_complement _A _B in
    _BArc `axiom_of_union` _ABrc

t_def_symmetric_difference = singleton 0 == def_symmetric_difference (t_setNat 10) (t_setPosNat 10)


{-
    Definition.

    A ⊆ B (A is a subset of B, A is included in B)

    A ⊆ B exactly if A and B are sets and for every x it is the case that if x is an element of A, then x is an element of B
-}


def_subset _A _B =
    let _A'elements = elems _A in
    and [ x `elementOf` _B | x <- _A'elements ]

t_def_subset = def_subset (t_setNat 10) (t_setNat 10)


{-
    Definition.

    A ⊂ B (A is a proper subset of B, A is properly included in B) exactly if A is a subset of B and A is not equal to B

    A `def_proper_subset` B (A is a proper subset of B, A is properly included in B) exactly if A `def_subset` of B and not . A `axiom_of_extensionality` B
-}

def_proper_subset _A _B
    | _A `axiom_of_extensionality` _B = False
    | otherwise = def_subset _A _B

t_def_proper_subset = def_proper_subset (t_setNat 10) (t_setNat 10)


{-
    Definition.

    A ⊇ B. A is a superset of B or A containers B.
-}

def_superset :: Ord a => Set a -> Set a -> Bool
def_superset = flip def_subset

t_def_superset = def_superset (t_setNat 10) (t_setNat 10)


{-
    Definition.

    A ⊃ B. A is a proper superset of B or A properly contains B.
-}

def_proper_superset :: Ord a => Set a -> Set a -> Bool
def_proper_superset = flip def_proper_subset

t_def_proper_superset = def_proper_superset (t_setNat 11) (t_setNat 10)


t_subset_and_superset =
    let _A = t_setNat 11 in
    let _B = t_setNat 10 in 
    (def_subset _B _A, def_superset _A _B)

t_proper_subset_and_superset =
    let _A = t_setNat 11 in
    let _B = t_setNat 10 in 
    (def_proper_subset _B _A, def_proper_superset _A _B)


{-
    Theorem.

    { } ⊆ A for every set A
    [] `def_subset` A for every set A

    Proof.

    If x is an element of { }... (anything, including "x is an element of A", follows).

    *** Warning ***

    Inclusion is not equivalent to membership: { } `def_subset` { } by the Theorem, but it is not the case that { } `elementOf` { }.
-}

theorem_empty_set_subset_of_every_set = def_subset empty (t_setNat 10)


{-
    Theorem.

    For all sets A, B. A ⊆ B exactly if A ∪ B = B.
    For all sets A, B. A `def_subset` B exactly if `axiom_of_union` B `axiom_of_extensionality` B
-}

theorem_not_included _A _B =
    _A `axiom_of_union` _B `axiom_of_extensionality` _B


{-
    Definition.

    Sets A and B are said to be disjoint exactly if for every x, either x is not an element of A or x is not an element of B or both. (no x belongs to both A and B)
-}

def_disjoint _A _B =
    let _A'elements = elems _A in
    let _B'elements = elems _B in
    L.null $ [ x | x <- _A'elements, x `elementOf` _B] ++ [ x | x <- _B'elements, x `elementOf` _A ]


{-
    Theorem.

    Sets A, B are disjoint exactly if A ∩ B = { }
    Sets A, B are def_disjoint exactly if A `axiom_of_union` B `axiom_of_extensionality` emptySet
-}

theorem_disjoint _A _B = (_A `axiom_of_union` _B) `axiom_of_extensionality` empty
theorem_disjoint' _A _B = (_A `theorem_intersection` _B) `axiom_of_extensionality` empty


{-
    Pairwise disjoint sets: A ∩ B = A ∩ C = B ∩ C = { }.
-}


{-
    Commutative laws.

    A ∩ B = B ∩ A
    A ∪ B = B ∪ A

    (A `theorem_intersection` B) `axiom_of_extensionality` (B `theorem_intersection` A)
    (A `axiom_of_union` B) `axiom_of_extensionality` (B `axiom_of_union` A)
-}

law_commutative _A _B =
    (_A `theorem_intersection` _B) `axiom_of_extensionality` (_B `theorem_intersection` _A)
    &&
    (_A `axiom_of_union` _B) `axiom_of_extensionality` (_B `axiom_of_union` _A)

t_law_commutative =
    let _A = t_setNat 10 in
    let _B = t_setPosNat 100 in
    law_commutative _A _B


{-
    Associative laws.

    (A ∩ B) ∩ C = A ∩ (B ∩ C)
    (A ∪ B) ∪ C = A ∪ (B ∪ C)
-}

law_associative _A _B _C =
    ((_A `theorem_intersection` _B) `theorem_intersection` _C) `axiom_of_extensionality` (_A `theorem_intersection` (_B `theorem_intersection` _C))
    && 
    ((_A `axiom_of_union` _B) `axiom_of_union` _C) `axiom_of_extensionality` (_A `axiom_of_union` (_B `axiom_of_union` _C))

law_associative _A _B _C =
    (_A ∩ _B) ∩ _C .=. (_A ∩ (_B ∩ _C))
    && 
    (_A ∪ _B) ∪ _C .=. (_A ∪ (_B ∪ _C))

t_law_associative =
    let _A = t_setNat 10 in
    let _B = t_setPosNat 100 in
    let _C = toSet $ take 200 oddNats in
    law_associative _A _B _C


{-
    Distributive laws.

    A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C)
    A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C)
-}

law_distributive _A _B _C =
    (_A `theorem_intersection` (_B `axiom_of_union` _C)) `axiom_of_extensionality` ((_A `theorem_intersection` _B) `axiom_of_union` (_A `theorem_intersection` _C))
    &&
    (_A `axiom_of_union` (_B `theorem_intersection` _C)) `axiom_of_extensionality` ((_A `axiom_of_union` _B) `theorem_intersection` (_A `axiom_of_union` _C))

law_distributive' _A _B _C =
    (_A ∩ _B) ∪ _C .=. ((_A ∩ _B) ∪ (_A ∩ _C))
    &&
    _A ∪ (_B ∩ _C) .=. ((_A ∪ _B) ∩ (_A ∪ _C))


{-
    Identity laws.

    A ∪ { } = A
    A ∩ V = A
-}

{-
law_identity _A Empty  = _A
law_identity _A Universe = _A
-}


{-
    Idempotence laws.

    A ∪ A = A
    A ∩ A = A
-}

law_idempotence _A =
    _A ∪ _A .=. _A
    &&
    _A ∩ _A .=. _A


{-
    Cancellation laws.

    A ∪ V = V
    A ∩ { } = { }
-}

law_cancellation _A =
--    _A ∪ Universe = Universe
    _A ∩ empty .=. empty


{-
    De Morgan's laws.

    (A ∩ B)^c A^c ∪ B^c
    (A ∪ B)^c = A^c ∩ B^c
-}


{-
    Double complement law.

    (A^c)^c = A
-}


{-
    Other complement laws.

    A^c ∩ A = { }
    A^c ∪ A = V
    V^c = { }
    { }^c = V
-}


{-
    Inclusion principles.

    A ⊆ B exactly if A = A ∩ B
    also exactly if B = B ∪ A.
-}

principle_of_inclusion _A _B =
    (_A ∩ _B) .=. _A
    &&
    (_B ∪ _A) .=. _B

t_princple_of_inclusion = principle_of_inclusion (t_setNat 10) (t_setNat 20)


{-
    Axiom of Set Union.

    If A is a set all of whose elements are sets, the set S
    U[A] = {x | for some B, x ∈ B and B ∈ A}, called the (set) union of A, exists.
-}
