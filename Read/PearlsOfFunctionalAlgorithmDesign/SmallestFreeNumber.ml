let find e arr =
    let n = Array.length arr in
    let rec find' n' =
        if (n' > n)
            then
                None
            else if (arr.(n') = e) then (Some n') else find' (n'+1)
    in find' 0

let rec range i j =
    match (i = j) with
    | true -> [i]
    | false -> i :: range (i+1) j

let rec zip lx ly =
    match lx, ly with
    | [], _ -> []
    | _, [] -> []
    | (x::xs), (y::ys) -> (x,y) :: zip xs ys

let rec from_assocs arr xs =
    match xs with
    | [] -> arr
    | ((i,e)::t) -> (Array.set arr i e; from_assocs arr t)

let check_list xs =
    let n = List.length xs + 1 in
    let arr = Array.make (n+1) false  in
    from_assocs arr (List.map (fun a -> (a, true)) (List.filter (fun a -> a < n) xs))

let find_smallest_array xs = find false (check_list xs)

let t = find_smallest_array [0;1;2;3;4;5;7;8;9;11]
let t' = find_smallest_array [0;100];;
