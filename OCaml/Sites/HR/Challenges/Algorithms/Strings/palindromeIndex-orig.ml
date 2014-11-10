open List;;
open String;;
open Printf;;

let rec read_strings n =
    match n with
    | 0 -> []
    | _ ->
        let s = read_line () in
        s :: read_strings (n-1)
    ;;

(*
let pal_index s =
    (*String.length s*)
    let quit = ref false in
    let index = ref 0 in
    let il = ref 0 in
    let ir = ref (String.length s) in
    let index = while not !quit do
        let cl = s.[!il] in
        let cr = s.[!ir] in
        if cl != cr then
            quit := true
    done in
    index
    ;;
*)

(*
 aaab
 a|aa|b

 baa
 b|a|a
 
 aaa
 a|a|a

 eeeeea
 e|eee|a
 
 aeeeee
 a|eee|e
*)

let rec pal_index_rec il ir s =
    match (il == ir)  with
    | true -> 0
    | false ->
        match (s.[il] = s.[ir]) with
        | true -> pal_index_rec (il+1) (ir-1) s
        | false -> 
            let c = s.[il+1] in
            if s.[il] == c
                then
                    ir
                else
                    il
    ;;

let pal_index s =
    let il = 0 in
    let ir = (String.length s) - 1 in
    pal_index_rec il ir s
    ;;

let pal_indexes strings =
    List.map (fun s -> pal_index s) strings
    ;;

let print_indexes indexes =
    List.map (fun i -> fprintf stdout "%d\n" i) indexes
    ;;

let challenge _ =
    let t = read_int () in
    let strings = read_strings t in
    let indexes = pal_indexes strings in
    let _ = print_indexes indexes in
    ()
    ;;

let main = challenge ();;
