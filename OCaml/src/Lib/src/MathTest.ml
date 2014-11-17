open OUnit;;
open Math;;

module MathTest =

 struct

  let ae = assert_equal;;

  let test_fib _ =
   assert_equal 0 (Math.fib 0);
   assert_equal 1 (Math.fib 1);
   assert_equal 6765 (Math.fib 20);; 

  let test_fact _ =
   assert_equal 1 (Math.fact 0);
   assert_equal 120 (Math.fact 5);;

  let test_times _ =
   assert_equal 0 (Math.times 0 5);
   assert_equal 0 (Math.times 5 0);
   assert_equal 50 (Math.times 5 10);;

  let test_double _ =
   assert_equal 10 (Math.double 5);;

  let test_induction_fact _ =
   assert_equal 1 (Math.induction_fact 0);
   ae 120 (Math.induction_fact 5);;

  let test_induction_sum_int _ =
   ae 0 (Math.induction_sum_int 0);
   ae 1 (Math.induction_sum_int 1);
   ae 210 (Math.induction_sum_int 20);;

  let test_induction_sum_sqr _ =
   ae 0 (Math.induction_sum_sqr 0);
   ae 1 (Math.induction_sum_sqr 1);
   ae 2870 (Math.induction_sum_sqr 20);;

  let suite = "OUnit" >:::
   [
    "test_fib" >:: test_fib;
    "test_fact" >:: test_fact;
    "test_times" >:: test_times;
    "test_double" >:: test_double;
    "test_induction_fact" >:: test_induction_fact;
    "test_induction_sum_int" >:: test_induction_sum_int;
    "test_induction_sum_sqr" >:: test_induction_sum_sqr;
   ];;

 end;;
