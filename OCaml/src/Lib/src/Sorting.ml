module Sorting =

 struct

  let rec quicksort pxs =
   match pxs with
   | [] -> []
   | (p::xs) -> quicksort (List.filter (fun x -> x < p) xs) @ [p] @ quicksort (List.filter (fun x -> x >= p) xs)
   ;;

  let rec quicksort' pxs =
   match pxs with
   | [] -> []
   | (p::xs) ->
    let lesser = List.filter (fun x -> x < p) xs in
    let greater = List.filter (fun x -> x >= p) xs in
    quicksort lesser @ [p] @ quicksort greater
   ;;

end;;
