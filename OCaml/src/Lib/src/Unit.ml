open OUnit;;
open Math;;
open MathTest;;

let k = Math.fib 0

let _ =
 run_test_tt_main MathTest.suite
 ;;
