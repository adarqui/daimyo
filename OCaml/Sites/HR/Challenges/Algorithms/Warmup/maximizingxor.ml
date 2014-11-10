open Scanf;;
open Printf;;
open List;;

let id x = x;;
let rec range i j =
    match (i>j) with
    | true -> []
    | false -> i :: range (i+1) j
    ;;

let pairs l r = concat (map (fun a -> map (fun b -> (a,b)) (range a r)) (range l r));;

let max'xor nums = fold_left (fun acc (i,j) -> max (i lxor j) acc) 0 nums;;

let main () =
    let _L = read_int () in
    let _R = read_int () in
    fprintf stdout "%d\n" (max'xor (pairs _L _R))
    ;;

let () = main ()
