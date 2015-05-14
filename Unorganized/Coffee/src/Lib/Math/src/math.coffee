fib = (n) ->
 if n < 2
  n
 else
  (fib n - 1) + (fib n - 2)

fact = (n) ->
 if n is 0
  1
 else
  n * fact n - 1
 

times = (n, m) ->
 n * m

double = (n) ->
 times n, 2

induction = (base, comb, n) ->
 if n <= 0
  base
 else
  comb n (induction base comb (n-1))

add = (x, y) -> x + y
mul = (x, y) -> x * y
sumsqr = (x, y) -> add (mul x x, y)

induction_fact = (n) -> induction 1, mul, n
induction_sum_int = (n) -> induction 0, add, n
induction_sum_sqr = (n) -> induction 0, sumsqr, n

exports.fib = fib
exports.fact = fact
exports.times = times
exports.double = double
exports.induction = induction
exports.induction_fact = induction_fact
exports.induction_sum_int = induction_sum_int
exports.induction_sum_sqr = induction_sum_sqr
