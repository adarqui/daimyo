#include "math.h"

int fib(int n) {
 if(n < 2) return n;
 else return fib(n-1) + fib(n-2);
}


int fact(int n) {
 if(n == 0) return 1;
 else return n * fact(n-1);
}


int times(int n, int m) {
 return n * m;
}


int double_(int n) {
 return times(n, 2);
}


int induction(int base, int (*comb)(int x, int y), int n) {
 if(n == 0) {
  return base;
 } else {
  return comb(n, induction(base, comb, n-1));
 }
}


int add(int x, int y) {
 return x + y;
}

int mul(int x, int y) {
 return x * y;
}

int sumsqr(int x, int y) {
 return add(mul(x,x),y);
}


int induction_fact(int n) {
 return induction(1, mul, n);
}

int induction_sum_int(int n) {
 return induction(0, add, n);
}

int induction_sum_sqr(int n) {
 return induction(0, sumsqr, n);
}
