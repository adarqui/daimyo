module Kategori (
) where

{-
class Category a b where
    id :: a -> a
    comp :: (a -> b) -> (b -> c) -> (a -> c)
-}

{-
data Category a b = Category
    idA :: a -> a
-}

id :: a -> a
id x = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
