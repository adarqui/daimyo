(*
no n+k patterns

let rec factNK = function
    | n+1 -> n
    | 0 -> 1
*)

let rec range x y =
    if (x == y) then [y]
    else (x :: range (x+1) y)

let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)

let mul = ( * )

let factFold n = List.fold_left mul 1 (range 1 n)

let rec len = function
    | [] -> 0
    | (h::t) -> 1 + len t

let rec sum = function
    | [] -> 0
    | (h::t) -> h + sum t

let rec join a b =
    match a, b with
    | [], ys -> ys
    | (x::xs), ys -> x :: join xs ys

let (<>) = join

let zip = List.map2 (fun a b -> (a,b))

let rec concat = function
    | [] -> []
    | (h::t) -> h <> concat t

let rec qsort = function
    | [] -> []
    | (h::t) -> qsort (List.filter (fun a -> a<h) t) @ [h] @ qsort (List.filter (fun a -> a>=h) t)

let t1 = [1;2;3] <> [4;5;6]

let rec copy = function
    | [] -> []
    | (h::t) -> h :: copy t

let rec inverse = function
    | [] -> []
    | ((x,y)::t) -> (y,x) :: inverse t

let merge a b = qsort (a @ b)

let rec (<!!>) l n =
    match l, n with
    | [], _ -> None
    | (x::_), 0 -> Some x
    | (_::xs), n -> (<!!>) xs (n-1)

let rec lookup n l =
    if (l = [])
        then None
        else
            let ((x,y)::t) = l in
            match (x == n) with
            | true -> Some y
            | false -> lookup n t

let rec count e l =
    match e, l with
    | _, [] -> 0
    | e, (h::t) ->
        let v = (if (e == h) then 1 else 0)
        in v + count e t

let rec drope e l =
    if (l = [])
        then []
        else
            let (h::t) = l in
            match (e == h) with
            | true -> drope e t
            | false -> h :: drope e t

let rec dropAlt l =
    List.map fst
    (List.filter (fun (x,y) -> y mod 2 > 0)
    (zip l (range 0 ((List.length l)-1))))

let rec extract = function
    | [] -> []
    | (h::t) ->
        match h with
        | None -> extract t
        | (Some h') -> h' :: extract t

let rec map f l =
    match l with
    | [] -> []
    | (h::t) -> f h :: map f t

let rec zipWith f xz yz =
    match f, xz, yz with
    | _, [], _ -> []
    | _, _, [] -> []
    | f, (x::xs), (y::ys) -> f x y :: zipWith f xs ys

let rec foldr f z l =
    match l with
    | [] -> z
    | (h::t) -> f h (foldr f z t)

let sumR = foldr ( + ) 0
let productR = foldr ( * ) 1
let and' = foldr ( && ) true
let or' = foldr ( || ) false
let factorial n = foldr ( * ) 1 (range 1 n)


type peano = Zero | Succ of peano

let one = Succ Zero
let three = Succ (Succ (Succ Zero))

(*
let extract a = function
    | Zero -> Zero
    | 
*)

let decr = function
    | Zero -> Zero
    | (Succ a) -> a

let rec add a b =
    if (a = Zero)
        then b
        else Succ (add (decr a) b)

let rec sub a b =
    match a, b with
    | a, Zero -> a
    | Zero, b -> Zero
    | (Succ a), (Succ b) -> sub a b

let rec equals a b =
    match a, b with
    | Zero, Zero -> true
    | Zero, _ -> false
    | _, Zero -> false
    | (Succ a), (Succ b) -> equals a b

let rec lt a b =
    match a, b with
    | _, Zero -> false
    | Zero, (Succ b) -> true
    | (Succ a), (Succ b) -> lt a b

(* Examples *)

let ex1 = copy [2]
let ex2 = inverse [(1,2);(3,4)]
let ex3 = merge [1;2;5;201;3;4;2;1] [2;4;8;5;4;6;2]
let ex4 =
    let l = [1;2;3] in
    [l <!!> 0, l <!!> 2, l <!!> 5]
let ex5 =
    let l = [(1,2);(5,3)] in
    [lookup 5 l; lookup 6 l]
let ex6 = count 1 [1;2;3;1;2;3;1]
let ex7 = drope 1 [1;2;3;1;2;3;1]
let ex8 = dropAlt (range 1 7)
let ex9 = extract [Some 3; None; Some 7]

let t1 = foldr ( + ) 0 [1;2;3]
