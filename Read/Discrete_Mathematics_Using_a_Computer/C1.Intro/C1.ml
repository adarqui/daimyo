open Big_int

(* helpers *)

let rec take n l =
    if (l == []) then [] else
    match n with
    | 0 -> []
    | _ -> List.hd l :: take (n-1) (List.tl l)

let rec drop n l =
    if (l == []) then [] else
    match n with
    | 0 -> l
    | _ -> drop (n-1) (List.tl l)

let rec zipWith f l1 l2 =
    if ([] == l1 || [] == l2)
        then []
        else (f (List.hd l1) (List.hd l2)) :: zipWith f (List.tl l1) (List.tl l2)
    
let zip l1 l2 = zipWith (fun a b -> (a, b)) l1 l2

let list_of_string s =
    let rec tolist i acc =
        match i with
        | 0 -> s.[i] :: acc
        | _ -> tolist (i-1) (s.[i] :: acc)
    in
        tolist (String.length s - 1) []

let rec range i j =
    match (i > j) with
    | true -> []
    | false -> i :: range (i + 1) j

let compose f g x = f (g x)

let contains c s = String.contains s c

(* chapter *)

let int_types = (0, Big_int.zero_big_int)

let bigint = Big_int.zero_big_int

let int_exp = 9*9
let integer_exp = string_of_big_int (power_big_int_positive_big_int (big_int_of_int 9) (big_int_of_int 100))

let int_div = 10 / 2
let float_div = 10.0 /. 2.0

let float_math = 10.0 +. 2.1 /. 5.6 -. 1.0 *. 2.0 ** 5.0

let bools = not (true && false || true)
let comparison = (1 < 2 && 2 < 3) && (3 >= 2 && 2 >= 1) != false

let c = 'A'
let _C = Char.uppercase c

let s = "abcdef"
let _S = String.uppercase s
let _S' = String.concat "" (List.map (fun c -> String.make 1 (Char.uppercase c)) (list_of_string s))
let _S'' = String.map (fun c -> Char.uppercase c) s

let strings = String.concat "" ["abc";"def";"xyz"]

let tup = (1, "string")
let fstTup = fst tup
let sndTup = snd tup

let num_list = [1,2,3,4,5]
let num_lists = [num_list, num_list, num_list]
let num_lists' = List.map (fun _ -> num_list) [1;2;3]

let cons = 1 :: 2 :: 3 :: 4 :: 5 :: []
let twolists = cons @ cons

let double x = x * x
let double' = function x -> x * x
let double'' = fun x -> x * x

let type_signature (x : int) = x

exception Wrecked

let pattern_matching x =
    match x with
    | 0 -> false
    | 1 -> true
    | _ -> raise Wrecked

let pattern_matching' = function
      0 -> false
    | 1 -> true
    | _ -> raise Wrecked

let match_tuple (x,y) = x == y

let twice f a = f (f a)
let twice_example = twice (fun a -> a + a) 5

let quadratic a b c =
    let d = sqrt (b**2.0 -. 4.0*.a*.c) in
    let x1 = ((b*.(-1.0)) +. d) /. (2.0*.a) in
    let x2 = ((b*.(-1.0)) -. d) /. (2.0*.a) in
    (x1,x2)

let list_index_example = List.nth [1;2;3;4;5] 2
let take_example =
    let l = [1;2;3] in
    (take 2 l, take 0 l, take 4 l)

let drop_example =
    let l = [1;2;3] in
    (drop 2 l, drop 0 l, drop 4 l)

let map_example = String.map (fun c -> Char.uppercase c) "the cat and dog"

let zip_example = zip [1;2;3] ['a';'b';'c']
let zipWith_example = zipWith (fun a b -> a + b) [1;2;3] [1;2;3]

let foldl_example = List.fold_left (fun acc x -> x + acc) 0 (range 1 10)
let foldr_example = List.fold_right (fun acc x -> x + acc) (range 1 10) 0

let compose_example = compose Char.uppercase Char.lowercase 'c'

(*let compose_list = (compose (::) (fun x -> Char.uppercase x) 'a') "bc"*)

type color = Red | Yellow | Blue

type booL = False | True

type animal = Cat of string | Dog of string | Rat

type ('a, 'b) animal' = Cat' of 'a | Dog' of 'b | Rat'

type 'a maybe = Nothing |Just of 'a

exception MaybeException

let fromJust = function
      Just a -> a
    | _ -> raise MaybeException

let maybePlus a b =
    if (a == Nothing)
        then b
    else if (b == Nothing)
        then a
    else
        Just ((fromJust a) + (fromJust b))
    

(* exercises *)

let ex1 =
    zip
    [
        [true && false];
        [true || false];
        [not false];
        [3 <= 5 && 5 <= 10]
    ]
    (range 1 10)

exception WhoCares

let ex2 () = raise WhoCares

let ex3 c =
    match c with
    | 'a' -> true
    | _ -> false

let ex4 s =
    match s with
    | "hello" -> true
    | _ -> false

let ex5 s =
    let len = String.length s in
    if (len > 0)
        then
            if (s.[0] == ' ')
                then String.sub s 1 (len-1)
                else s
        else
            s

exception ToBoolException
       
let ex6 =
    let toBool = function
          0 -> false
        | 1 -> true
        | _ -> raise ToBoolException
    in
    List.map toBool

let ex6_example = ex6 [0;1;0;1;0;1;1;1]

let ex7 = contains '0'
let ex7_example = ex7 "Aleph"
let ex7_example' = ex7 "Aleph0"

let ex9_addjust = zipWith maybePlus
let ex9_addjust_example = ex9_addjust [Just 2; Nothing; Just 3] [Nothing; Nothing; Just 5]

type ex10 = Metal1 | Metal2 | Metal3 | Metal4 | Metal5 | Metal6

type 'a ex11 = Coin of 'a

type stuff = Bool of bool | Char of char | Int of int 
type ('a, 'b, 'c, 'd) tup4 = Tup4 of 'a * 'b * 'c * 'd

let ex14_quadratic a b c =
    let discrim = b**2.0 -. 4.0*.a*.c in
    let quad op = (op (b*.(-1.0)) (sqrt discrim))/.(2.0*.a) in
    if (discrim < 0.0)
        then Nothing
        else Just (quad (-.), quad (+.))

let bitOr b1 b2 =
    if (b1 == 0 && b2 == 0)
        then 0
        else 1

let bitAnd b1 b2 =
    if (b1 == 1 && b2 == 1)
        then 1
        else 0

let bitwiseAnd = zipWith bitAnd
let bitwiseOr = zipWith bitOr

let ex20 = List.map (fun a -> fromJust a) (List.filter (fun a -> a != Nothing) [Just 3;Nothing;Just 4])
