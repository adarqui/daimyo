package math

import (
 "testing"
)

func try(t *testing.T, b bool, m string) {
 if b == false {
  t.Error("Failure: " + m)
 } else {
  t.Log("Success: " + m)
 }
}


func Test_Fib(t *testing.T) {
 try(t, Fib(0) == 0, "fib(0) == 0")
 try(t, Fib(1) == 1, "fib(1) == 1")
 try(t, Fib(20) == 6765, "fib(20) == 6765")
}


func Test_Fact(t *testing.T) {
 try(t, Fact(0) == 1, "fact(0) == 1")
 try(t, Fact(5) == 120, "fact(5) == 120")
}


func Test_Times(t *testing.T) {
 try(t, Times(0, 5) == 0, "times(0, 5) == 0")
 try(t, Times(5, 0) == 0, "times(5, 0) == 0")
 try(t, Times(5, 10) == 50, "times(5, 10) == 50")
}


func Test_Double(t *testing.T) {
 try(t, Double(5) == 10, "double(5) == 10")
}


func Test_Induction_Fact(t *testing.T) {
 try(t, Induction_Fact(0) == Fact(0), "induction_fact(0) == fact(0)")
 try(t, Induction_Fact(1) == Fact(1), "induction_fact(1) == fact(1)")
 try(t, Induction_Fact(5) == Fact(5), "induction_fact(5) == fact(5)")
}

func Test_Induction_Sum_Int(t *testing.T) {
 try(t, Induction_Sum_Int(0) == 0, "induction_sum_int(0) == 0")
 try(t, Induction_Sum_Int(1) == 1, "induction_sum_int(1) == 1")
 try(t, Induction_Sum_Int(20) == 210, "induction_sum_int(20) == 210")
}

func Test_Induction_Sum_Sqr(t *testing.T) {
 try(t, Induction_Sum_Sqr(0) == 0, "induction_sum_sqr(0) == 0")
 try(t, Induction_Sum_Sqr(1) == 1, "induction_sum_sqr(1) == 1")
 try(t, Induction_Sum_Sqr(20) == 2870, "induction_sum_sqr(20) == 2870")
}
