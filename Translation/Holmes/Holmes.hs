{-# LANGUAGE ParallelListComp #-}

module Holmes (
) where

import Data.Set
import qualified Data.List as L

emptyBool = empty :: Set Bool
emptyNat = empty :: Set Int

{-
    ***
    Chapter 2: The Set Concept
    ***
-}

{-
    Axiom of Extensionality: if A and B are sets, and for each x, x is an element of A if and only if x is an element of B, then A = B.
-}

axiom_of_extensionality :: (Eq a, Ord a) => Set a -> Set a -> Bool
axiom_of_extensionality _A _B
    | size _A == size _B =
        let _A'elements = elems _A in
        let _B'elements = elems _B in
        and [ member x' _A && member x' _B | x <- _A'elements , x' <- _B'elements]
    | otherwise = False


t_setNat n = fromList [0..n]
t_setPosNat n = fromList [1..n]

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
