{- source: http://www.haskellforall.com/2012/08/the-category-design-pattern.html -}

{-# LANGUAGE NullaryTypeClasses #-}

class Compose where
    (|.|) :: (b -> c) -> (a -> b) -> (a -> c)

instance Compose where
    f |.| g = f . g

associative f g h = (f . g) . h == f . (g . h)

f = (+1)
g = (^2)
h = (*3)

fgh = f . g . h

t_comp = ((+1) |.| (+2)) 3
t_fgh = fgh 2

