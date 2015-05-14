"use strict;";

var fib = function(n) {
 if (n < 2) {
  return n;
 }
 return fib(n-1) + fib(n-2);
}


var fact = function(n) {
 if (n == 0) {
  return 1;
 }
 return n * fact(n-1);
}

var induction = function(base, comb, n) {
 if(n == 0) {
  return base;
 } else {
  return comb(n, induction(base, comb, n-1));
 }
}

var induction_fact = function(n) {
 return induction(1, function(x, y) { return x * y; }, n);
}

var induction_sum_int = function(n) {
 return induction_sum_int(0, function(x, y) { return x + y; }, n);
}

var induction_sum_sqr = function(n) {
 return induction_sum_sqr(0, function(x, y) { return x * x + y; }, n);
}

module.exports = {
 fib : fib,
 fact : fact,
 induction : induction,
 induction_fact : induction_fact,
 induction_sum_int : induction_sum_int,
 induction_sum_sqr : induction_sum_sqr
}
