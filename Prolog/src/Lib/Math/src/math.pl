:- module(math, [fib/2, fact/2, times/3, double/2, inductive/4, inductive_fact/2, inductive_sum_int/2, inductive_sum_sqr/2]).
:- include('/data/lib/crisp/lib/crisp_includes').


describe(fib/2,
 [
  fib(0,0) ,
  fib(1,1) ,
  fib(20,6765)
 ]).

describe(fact/2,
 [
  fact(0,1) ,
  fact(5,120)
 ]).

describe(times/3,
 [
  times(0,5,0) ,
  times(5,0,0) ,
  times(5,10,50)
 ]).

describe(double/2,
 [
  double(5,10)
 ]).

describe(inductive_fact/2,
 [
  inductive_fact(5, 120)
%  inductive_fact(5, once(fact(5, R)))
 ]).

describe(inductive_sum_int/2,
 [
  inductive_sum_int(20, 210)
 ]).

describe(inductive_sun_sumsqr/2,
 [
  inductive_sum_sqr(20, 2870)
 ]).

fib(0, R) :- R is 0.
fib(1, R) :- R is 1.
fib(N, R) :-
 N1 is N - 1, N2 is N - 2,
 fib(N1, R1), fib(N2, R2),
 R is R1 + R2.
 

fact(0, R) :- R is 1.
fact(N, R) :-
 N1 is N - 1,
 fact(N1, R1),
 R is N * R1.


times(N, M, R) :-
 R is N * M.


double(N, R) :-
 times(N, 2, R).

inductive(Base, Comb, 0, R) :- R is Base.
inductive(Base, Comb, N, R) :-
 N1 is N - 1,
 inductive(Base, Comb, N1, R1),
 call(Comb, N, R1, R2),
 R is R2.

add(X, Y, R) :-
 R is X + Y.

mul(X, Y, R) :-
 R is X * Y.

sumsqr(X, Y, R) :-
 R is X * X + Y.

inductive_fact(N, R) :-
 inductive(1, mul, N, R).

inductive_sum_int(N, R) :-
 inductive(0, add, N, R).

inductive_sum_sqr(N, R) :-
 inductive(0, sumsqr, N, R).
