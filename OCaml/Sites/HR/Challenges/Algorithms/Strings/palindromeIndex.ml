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

let rec pal_index_rec il ir s =
    if (il >= ir)
    then (-1)
    else
        if (s.[il] = s.[ir])
            then pal_index_rec (il+1) (ir-1) s
            else
                match (s.[il] = s.[il+1]) with
                | true -> ir
                | false ->
                    if (pal_index_rec (il+2) (ir-1) s == (-1))
                        then il
                        else ir
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
