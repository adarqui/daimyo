open List;;

module type Rose =
    sig
(*        type 'a rose : Empty | Node of 'a * 'a rose list *)
    end;;

type 'a rose = Empty | Node of 'a * 'a rose list

let rec belongs e = function
      Empty -> false
    | Node (v, bs) -> (e = v) || (List.exists (belongs e) bs)

let rec height =
    let max_list l = List.fold_left max 0 l in
        function
              Empty -> 0
            | Node (_, bs) -> 1 + (max_list (List.map height bs))

(* FINISHME *)
