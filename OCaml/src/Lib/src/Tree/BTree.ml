module type BTree =
    sig
        type 'a btree = Empty | Node of 'a btree * 'a * 'a btree
        val list_of_btree : 'a btree -> 'a list
        val btree_of_list : 'a list -> 'a btree
        val insert : 'a -> 'a btree -> 'a btree
        val sort : 'a list -> 'a btree
    end;;

    type 'a btree = Empty | Node of 'a btree * 'a * 'a btree

let rec insert e = function
      Empty -> Node (Empty, e, Empty)
    | Node (lb, x, rb) -> if e < x then Node (insert e lb, x, rb) else Node (lb, x, insert e rb)
    ;;

let rec list_of_btree = function
      Empty -> []
    | Node (lb, x, rb) -> (list_of_btree lb) @ (x :: list_of_btree rb)
    ;;

let rec btree_of_list = function
      [] -> Empty
    | (h::t) -> insert h (btree_of_list t)
    ;;

let sort l = list_of_btree (btree_of_list l);;
