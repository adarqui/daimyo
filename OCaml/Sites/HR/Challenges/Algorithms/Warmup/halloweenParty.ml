open Scanf;;
open Printf;;
open List;;

let rec replicate n (f : unit -> unit) =
    match n with
    | 0 -> ()
    | _ -> let _ = f () in replicate (n - 1) f;;

let pr n =
    fprintf stdout "%d\n" n;;

let solve _ =
    let n = read_int () in
    match (n mod 2 = 0) with
    | true -> pr ((n/2)*(n/2))
    | false -> pr ((n/2)*(n/2+1))

let challenge _ =
    let t = read_int () in
    replicate t (fun _ -> solve ());;

let main = challenge ();;
