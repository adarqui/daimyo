#[allow(dead_code)]
fn fib(n : int) -> int {
 match n {
  0 => 0,
  1 => 1,
  _ => fib(n-1) + fib(n-2)
 }
}

#[test]
fn fib_test() {
 assert!(fib(0) == 0);
 assert!(fib(1) == 1);
 assert!(fib(5) == 5);
 assert!(fib(20) == 6765);
}


#[allow(dead_code)]
fn fact(n : int) -> int {
 match n {
  0 => 1,
  _ => n * fact(n-1)
 }
}

#[test]
fn fact_test() {
 assert!(fact(0) == 1);
 assert!(fact(5) == 120);
}


#[allow(dead_code)]
fn times(n : int, m : int) -> int {
 n * m
}

#[test]
fn times_test() {
 assert!(times(0, 5) == 0);
 assert!(times(5, 10) == 50);
}


#[allow(dead_code)]
fn double(n : int) -> int {
 times(n, 2)
}

#[test]
fn double_test() {
 assert!(double(5) == 10);
}


#[allow(dead_code)]
/*
fn inductive(base : int, comb : |x : int, y : int| -> int, n : int) -> int {
 match n {
  0 => base,
  _ => comb(n, inductive(base, comb, n-1))
 } 
}
*/
#[allow(dead_code)]
fn inductive(base : int, comb : fn(x : int, y : int) -> int, n : int) -> int {
 match n {
  0 => base,
  _ => comb(n, inductive(base, comb, n-1))
 } 
}

#[allow(dead_code)]
fn mul(x : int, y : int) -> int {
 x * y
}

#[allow(dead_code)]
fn add(x : int, y : int) -> int {
 x + y
}

#[allow(dead_code)]
fn sumsqr(x : int, y : int) -> int {
 x * x + y
}

#[allow(dead_code)]
fn inductive_fact(n : int) -> int {
// let mul = &fn(x : int, y : int) -> int = |x, y| x * y;
 inductive(1, mul, n)
}

#[allow(dead_code)]
fn inductive_sum_int(n : int) -> int {
 inductive(0, add, n)
}

#[allow(dead_code)]
fn inductive_sum_sqr(n : int) -> int {
 inductive(0, sumsqr, n)
}


#[test]
fn inductive_fact_test() {
 assert!(120 == inductive_fact(5));
}

#[test]
fn inductive_sum_int_test() {
 assert!(210 == inductive_sum_int(20));
}

#[test]
fn inductive_sum_sqr_test() {
 assert!(2870 == inductive_sum_sqr(20));
}
