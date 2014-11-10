module Misc.MathTest (
 test,
 fib_asserts,
 fact_asserts,
 times_asserts,
 double_asserts,
 induction_fact_asserts,
 induction_sumInt_asserts,
 induction_sumSqr_asserts
) where

import Control.Exception (assert)
import Control.Monad (forM_)
import Control.Exception

import Misc.Math

assertError :: Bool -> String -> String
assertError predicate v
  | predicate = "Assertion passed: " ++ v
  | otherwise = throw (AssertionFailed ("Assertion failed: " ++ v))

fib_asserts =
 [
  assertError (fib 0 == 0) "fib 0 == 0",
  assertError (fib 1 == 1) "fib 1 == 1",
  assertError (fib 20 == 6765) "fib 20 == 6765"
 ]

fib_failed_asserts =
 [
  assertError (fib 0 == 100) "fib 0 == 100",
  assertError (fib 1 == 100) "fib 1 == 100",
  assertError (fib 20 == 100) "fib 20 == 100"
 ]

fact_asserts =
 [
  assertError (fact 0 == 1) "fact 0 == 1",
  assertError (fact 5 == 120) "fact 5 == 120"
 ]

times_asserts =
 [
  assertError (times 0 5 == 0) "times 0 5 == 0",
  assertError (times 5 0 == 0) "times 5 0 == 0",
  assertError (times 5 10 == 50) "times 5 10 == 50"
 
 ]

double_asserts =
 [
  assertError (double 5 == 10) "double 5 == 10"
 ]

induction_fact_asserts =
 [
  assertError (induction_fact 0 == 1) "induction_fact 0 == 1",
  assertError (induction_fact 5 == 120) "induction_fact 5 == 120"
 ]

induction_sumInt_asserts =
 [
  assertError (induction_sumInt 0 == 0) "induction_sumInt 0 == 0",
  assertError (induction_sumInt 1  == 1) "induction_sumInt 1 == 1",
  assertError (induction_sumInt 20  == 210) "induction_sumInt 20 == 210"
 ]

induction_sumSqr_asserts =
 [
  assertError (induction_sumSqr 0  == 0) "induction_sumSqr 0 == 0",
  assertError (induction_sumSqr 1  == 1) "induction_sumSqr 1 == 1",
  assertError (induction_sumSqr 20  == 2870) "induction_sumSqr 20 == 2870"
 ]

passed :: String -> String
passed m = "Test passed: " ++ m

test :: IO ()
test = do
-- forM_ fib_failed_asserts (putStrLn . passed)
 forM_ fib_asserts (putStrLn . passed)
 forM_ fact_asserts (putStrLn . passed)
 forM_ times_asserts (putStrLn . passed)
 forM_ double_asserts (putStrLn . passed)
 forM_ induction_fact_asserts (putStrLn . passed)
 forM_ induction_sumInt_asserts (putStrLn . passed)
 forM_ induction_sumSqr_asserts (putStrLn . passed)
