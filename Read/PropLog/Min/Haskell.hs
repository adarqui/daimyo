module PropLog.Min.Haskell (
) where

import Prelude hiding (not, and, or)

data Prop = T | F deriving (Show, Read, Eq)

{-
    The only logical operators allowed are: not, implies
-}

not T = F
not F = T

{-
    T T = T
    T F = F
    _ _ = T
-}
implies T F = F
implies _ _ = T

{-
    and:

    T T = T
    F F = T
    _ _ = F

    r = a & b

    a b r (a implies (not b))
    T T T  T              T
                      F
             F
        T
    F F    F              F
                      T
             T 
         F
    T F    T              F
                      T
             T
         F 
    F T    F              T
                      F
             T
         F
-}
and a b = not $ a `implies` (not b)

{-
    or:

    T T = T
    T F = T
    F T = T
    F F = F

    r = a or b
    a b = ((not a) implies b 
    T T =       T          T
            F     
                   T
    T F =       T          F
            F
                   T
    F T =       F          T
            T
                   T
    F F =       F          F
            T
                   F
-}
or a b = not a `implies` b

{-
    iff:
-}
