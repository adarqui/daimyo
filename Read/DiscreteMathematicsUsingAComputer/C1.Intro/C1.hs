import Data.List
import Data.Ratio
import Data.Char

int_bounds :: (Int, Int)
int_bounds = (minBound :: Int, maxBound :: Int)

int_types :: (Int, Integer)
int_types = (1 :: Int, 1 :: Integer)

int_exp = 9^9
integer_exp = 9**100

int_div = 10 `div` 2
double_div = 10 / 2

float_math = 10.0 + 2.1 / 5.6 - 1.0 * 2.0 ** 5.0

ratio = 2/3 :: Ratio Integer
denomRatio = denominator ratio
numRatio = numerator ratio

bools = not (True && False || True)
comparison = ((1 < 2 && 2 < 3) && (3 >= 2 && 2 >= 1)) /= False

c = 'A'
_C = toUpper c

s = "abcdef"
_S = map toUpper s

strings = "abc" ++ "def" ++ "xyz"

tup = (1, "string")
fstTup = fst tup
sndTup = snd tup

num_list = [1,2,3,4,5]
num_lists = replicate 3 num_list

alphabet = ['a'..'z']
alphabet' = map toUpper alphabet
base10 = [0..10]

cons = 1 : 2 : 3 : 4 : 5 : []
twolists = cons ++ cons

list_comprehension = [ x * x | x <- [1..10] ]
list_comprehensions = [ (x, y) | x <- list_comprehension, y <- list_comprehension ]

double x = x * x
lambda_double = \x -> x * x

type_signature :: Int -> Int
type_signature int = int

pattern_matching 0 = False
pattern_matching 1 = True

pattern_matching' x =
    case x of
        0 -> False
        1 -> True

pattern_matching'' x
    | x == 0 = False
    | x == 1 = True

match_tuple (x,y) = x == y

twice :: (a->a) -> a -> a
twice f a = f (f a)

quadratic a b c =
    let d = sqrt (b^2 - 4*a*c)
        x1 = (-b + d) / (2*a)
        x2 = (-b - d) / (2*a)
    in (x1,x2)

list_index_example = [1..5] !! 2

take_example =
    let l = [1,2,3] in
    (take 2 l, take 0 l, take 4 l)

drop_example =
    let l = [1,2,3] in
    (drop 2 l, drop 0 l, drop 4 l)

map_example = map toUpper "the cat and dog"

zip_example = zip [1,2,3] "abc"
zipWith_example = zipWith (+) [1,2,3] [1,2,3]

foldl_example = foldl (+) 0 [1..10]
foldr_example = foldr (+) 0 [1..10]

compose_example = (toUpper . toLower) 'c'
compose_list = ((:) . toUpper) 'a' "bc"

data Colour = Red | Yellow | Blue

data Animal = Cat String | Dog String | Rat

data Animal' a b = Cat' a | Dog' b | Rat'

-- exercises

ex1 = zip [True && False, True || False, not False, 3 <= 5 && 5 <= 10, 3<= 20 && 20 <= 10, False == True, 1 == 1, 1 /= 2, 1 /= 1] [1..]

ex2 =
    [
        [null [x | x <- [1,2,3], False]],
        [not (x && y) | x <- [False, True], y <- [False, True]],
        [x || y | x <- [False, True], y <- [False, True], x /= y]
    ]

ex3 c
    | c == 'a' = True
    | otherwise = False 

ex4 "hello" = True
ex4 _ = False

ex5 s@(sh:st)
    | sh == ' ' = st
    | otherwise = s

ex6 = map toBool
    where
        toBool n
            | n == 0 = False
            | n == 1 = True
            | otherwise = error "toBool: must be 0 or 1"

ex6_example = ex6 [0,1,0,1,0,1,1,0]

ex7 = any (=='0')
ex7_example = ex7 "Aleph"
ex7_example' = ex7 "Aleph0"

data List a = Cons a | Nil deriving (Show)

data Foldr op base list = Foldr op base list deriving (Show)

data Op a b = Max a b deriving (Show)

--foldr_unfold = Foldr max 0
foldr_unfold = Foldr Max 0 (Max 1 (Max 5 (Max 3 0)))

instance (Num a) => Num (Maybe a) where
    (+) Nothing b = b
    (+) a Nothing = a
    (+) (Just a) (Just b) = Just (a + b)

ex9_addJust = zipWith (+)

ex9_addJust_example = ex9_addJust [Just 2, Nothing, Just 3] [Nothing, Nothing, Just 5]

data Ex10 = Metal1 | Metal2 | Metal3 | Metal4 | Metal5 | Metal6 deriving (Eq)

data Ex11 a = Coin a

data Stuff = BOOL Bool | CHAR Char | INT Int

type Tup4 a b c d = (a,b,c,d)

ex14_quadratic a b c =
    case (discrim < 0) of
        True -> Just (quad (-), quad (+))
        False -> Nothing
    where
        discrim = b^2 - 4*a*c
        quad op =(-b `op` sqrt discrim)/(2*a)

bitOr :: Int -> Int -> Int
bitOr 0 0 = 0
bitOr _ _ = 1

bitAnd :: Int -> Int -> Int
bitAnd 1 1 = 1
bitAnd _ _ = 0

bitwiseAnd = zipWith bitAnd
bitwiseOr = zipWith bitOr

ex16 = bitwiseAnd

ex20 = map (\(Just a) -> a) $ filter (/= Nothing) [Just 3, Nothing, Just 4]

ex21 n = filter (>n)

ex22 ints int = map fst $ filter (\(f,s)->s==int) $ zip [1..] ints

-- tired going to pass out
