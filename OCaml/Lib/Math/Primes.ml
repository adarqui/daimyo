open List;;

module type Primes =
    sig
       val sieve : int list
    end;;

(*
let rec inf n = n :: inf (n + 1);;

let sieve_filter h t = filter (fun x -> (x mod h) > 0) t;;

let rec sieve' (h::t) = h :: sieve' (sieve_filter h t);;

let sieve = sieve' (inf 2);;
*)

let rec sum = fold_left (fun acc i -> i + acc) 0;;

let rec take n (h::t) =
    match n with
    | 0 -> []
    | _ -> h :: take (n - 1) t;;

let rec upto i j =
    match (i > j) with
    | true -> []
    | false ->  i :: upto (i+1) j;;

let sieve_filter h t = filter (fun x -> (x mod h) > 0) t;;

let rec sieve2 l =
    match l with
    | [] -> []
    | (h::t) -> h :: sieve2 (sieve_filter h t);;

let sieve = sieve2 (upto 2 10000);;

let primeSum n = sum (take n sieve);;
