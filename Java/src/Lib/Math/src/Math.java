package com.example.math;

public class Math {

 public static int fib(int n) {
  if (n < 2) {
   return n;
  }
  return fib(n-1) + fib(n-2);
 }

 public static int fact(int n) {
  if(n == 0) {
   return 1;
  } else {
   return n * fact(n-1);
  }
 }

}
