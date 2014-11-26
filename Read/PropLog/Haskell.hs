module PropLog.Haskell (
) where

import Prelude hiding (and,or,not)

data Prop = T | F deriving (Show, Read, Eq)

{-
Conjunction: The conjunction of two statements α and β, written in PL as '(α & β)', is true if both α and β are true, and is false if either α is false or β is false or both are false
-}

and :: Prop -> Prop -> Prop
and T T = T
and _ _ = F

{-
Disjunction: The disjunction of two statements α and β, written in PL as '(α v β)', is true if either α is true or β is true, or both α and β are true, and is false only if both α and β are false.
-}
or F F = F
or _ _ = T

{-
xor, such that '(α xor β)' would be regarded as true if α is true and β is false, or α is false and β is true, but would be regarded as false if either both α and β are true or both α and β are false.
-}
xor T F = T
xor F T = T
xor _ _ = F

{-
xor = not (a implies b)
-}
xor' a b = not $ a `iff` b


{-
Material Implication: This truth-function is represented in language PL with the sign '→'. A statement of the form  '(α → β)', is false if α is true and β is false, and is true if either α is false or β is true (or both).
-}
implies T F = F
implies _ _ = T

{-
Material Equivalence: This truth-function is represented in language PL with the sign '↔'. A statement of the form  '(α ↔ β)' is regarded as true if α and β are either both true or both false, and is regarded as false if they have different truth-values.
-}

iff a b
    | a == b = T
    | otherwise = F


{-
Negation: The negation of statement α, simply written '¬α' in language PL, is regarded as true if α is false, and false if α is true. 
-}

not T = F
not F = T
