module List (
 List(..),
 digits
) where

infixr 5 +++

data List a = Nil | Cons a (List a)

digits :: List Int
digits = Cons 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 Nil)))))))))

len :: List a -> Int
len Nil = 0
len (Cons _ y) = 1 + len y

hd :: List a -> a
hd Nil = error "Empty list"
hd (Cons x _) = x

tl :: List a -> List a
tl Nil = error "Empty list"
tl (Cons _ Nil) = Nil
tl (Cons _ y) = y

-- rev (slow)
-- rev (fast)
-- map

join :: List a -> List a -> List a
join Nil y = y
join (Cons x xs) ys = Cons x (join xs ys)

(+++) = join

inductLast :: List a -> a
inductLast Nil = error "Empty list"
inductLast (Cons x Nil) = x
inductLast (Cons x y) = inductLast y

--narrowLast x | x =:= y++[e] = e where y,e free
narrowLast x | x =:= y +++ (Cons e Nil) = e where y,e free
