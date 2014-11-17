module Math =

 struct
  let rec fib n =
   match n with
   | 0 -> 0
   | 1 -> 1
   | _ -> fib (n - 1) + fib (n - 2) ;;


  let rec fact n =
   match n with
   | 0 -> 1
   | _ -> n * fact (n - 1) ;;


  let times n m =
   n * m;;

  let double n =
   times n 2;;

  let rec induction base comb n =
   if n <= 0
    then base
    else comb n (induction base comb (n - 1))
   ;;

  let induction_fact n = induction 1 (fun x y -> x * y) n;;
  let induction_sum_int n = induction 0 (fun x y -> x + y) n;;
  let induction_sum_sqr n = induction 0 (fun x y -> x * x + y) n;;

 end;;
