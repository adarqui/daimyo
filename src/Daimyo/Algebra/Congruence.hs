module Daimyo.Algebra.Congruence (
    isCongruent,
    congruenceProof
) where

isCongruent a b n =
    let
        (q,r) = (a - b) `quotRem` n
    in
        r == 0

congruenceProof a b c n =
    let
        reflexive = isCongruent a a n && isCongruent b b n
        symmetric = isCongruent a b n && isCongruent b a n
        transitive = isCongruent a b n && isCongruent b c n && isCongruent a c n
    in
        [("reflexive", reflexive), ("symmetric", symmetric), ("transitive", transitive)]

congruenceProof' a b c d n =
    let
        additive = isCongruent a b n && isCongruent c d n && isCongruent (a + c) (b + d) n
        multiplicative = isCongruent a b n && isCongruent c d n && isCongruent (a*c) (b*d) n
    in
        [("additive", additive), ("multiplicative", multiplicative)]

t_isCongruent'1 = isCongruent 23 1 11
t_isCongruent'2 = isCongruent 23 2 7
t_isCongruent'3 = isCongruent 23 (-2) 25
t_isCongruent'1'False = isCongruent 23 1 10

t_congruenceProof = congruenceProof 23 1 12 11
t_congruenceProof' = congruenceProof' 23 1 23 12 11
