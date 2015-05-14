-module(math).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1,fact/1,inductive/3,inductive_fact/1,inductive_sum_int/1,inductive_sum_sqr/1]).


fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).


fact(0) -> 1;
fact(N) -> N * fact(N - 1).


times(N, M) -> N * M.
double(N) -> times(N, 2).


inductive(Base, _, 0) -> Base;
inductive(Base, Comb, N) -> Comb (N, inductive(Base, Comb, N - 1)).

inductive_fact(N) -> inductive(1, fun (X, Y) -> X * Y end, N).
inductive_sum_int(N) -> inductive(0, fun (X, Y) -> X + Y end, N).
inductive_sum_sqr(N) -> inductive(0, fun (X, Y) -> X * X + Y end, N).

%
% TESTS
%

fib_0_test() -> 0 = fib(0).
fib_1_test() -> 1 = fib(1).
fib_5_test() -> 5 = fib(5).
fib_20_test() -> 6765 = fib(20).
times_0_a_test() -> 0 = times(0, 5).
times_a_0_test() -> 0 = times(5, 0).
times_5_10_test() -> 50 = times(5, 10).
double_5_test() -> 10 = double(5).
inductive_fact_test() -> 120 = inductive_fact(5).
inductive_sum_int_test() -> 210 = inductive_sum_int(20).
inductive_sum_sqr_test() -> 2870 = inductive_sum_sqr(20).
