**Wave Exercises**

**Set Theory**
- "find the indicated intersection, union, difference, etc if possible" of randomly generated sets
- "express the following sets of numbers using interval notation" , "write the set using interval notation" of randomly generated set notations
- "use the blank venn diagram for A, B, C ... shade in the following sets" of randomly generated sets and operations among them
- venn diagram generator library
- Simple set operation generator

**Algebra**
+ Fractions: Classes for addition, subtraction, multiplication, division of fractions (of char, int, etc): https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/Fraction.hs
- Algebraic data structures for logarithms and base/exponents, and instances of classes to add/mul/div/etc.
- "simplify": fractions, order of operations, based on various laws of arithmetic. Should be able to show which law at each step: distribute the negative, equivalent fractions, multiplication of fractions, addition and subtraction of fractions etc.
- "matrix generation": generate matrices
- "matrix operations": generate matrics and then generate operations on matrices
+ Congruence Examples: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/Congruence.hs
+ Congruent Clock arithmetic: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/Congruence/Clock.hs
- Simple Addition, Subtraction, Multiplication, Division generator
- Simple fraction generator
+ Simple LCM Generator: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/Quiz/LCM.hs
+ Simple GCD Generator: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/Quiz/GCD.hs
- Simple Linear equation generator
- Simple System of equations generator
- Simple logarithm, exponents generator

**Abstract Algebra**
+ Binomial coefficients function: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/AbstractAlgebra/Polynomial.hs
+ Pascal triangle data: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/AbstractAlgebra/Polynomial.hs
- Addition modulo 2 for binary sequences of length n

**Calculus**
+ Experiment with limits: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Calculus/Exercises.hs

**Combinatorics**
- Random data to form graph structures
- Puzzle: How many regions of N intersecting lines form (with no points of the lines intersecting twice).
- Use cases of permutations & combinations

**Concrete Mathematics**
- Recurrence examples
- Generate normal form from various patterns of data
- Simple Sums generator

**Number Theory**
+ Primes, Prime sieve, Primes trial by division, Composites, Prime Factors: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime.hs
+ GCD: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/GCD.hs
- Express the fundamental axiom of mathematrics (every positive integer is a product of two primes): https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/TheoremOfMathematics.hs
+ Euclid (GCD using subtraction) explain via WriterT: "gcd'Explain" @ https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/GCD.hs
+ Euclid (GCD using modulus) explain via WriterT: "gcd'Explain" @ https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/GCD.hs
+ Prime Factors explain via WriterT: "primeFactors'Explain" @ https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime.hs
- Simple Prime / isPrime generator
- Simple divisibility test generator: is divisible by 2, 3, 5, 7, 11, ..?
- Prime/CoPrime Matrix

**Geometry**
- Express propositions/axioms from C1
- Generate diagrams of triangles using sage/haskell
+ Simple pythagorean theorem generator: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Quiz/Pythagorean.hs
+ Pythagorean Theorem 'solver': https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Pythagorean.hs
+ Pythagorean Triple Generator (triples): https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Pythagorean.hs

**Topology**
- Define a metric space

**Linear Algebra**
- Identify various fields

**Matrix**
- Various objects (strings, lists of numbers etc) stored in matrices and operations on them.
- Simple matrix generator

**Statistics**
+ Collecting data via /dev/<...>: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Random.hs
- Generating statistics on random data/strings/etc (sample, mean, variance, correlation stddev etc)

**Group Theory**
+ Caylay tables for multiplicative/additive groups: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/GroupTheory/Tables.hs
- Examples of semigroups
- Examples of monoids
- Examples of p-groups
- Examples of albelian groups

**Mathematics for Computer Science**
+ Logic tables via combinations: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Logic/Table.hs
+ Propositional Logic Examples: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Logic/Examples.hs
- Well Ordered Principle examples
- Induction Examples

**Logic**
- Simple Propositional Logic generator

**Tables**
- Power Table: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Table/Power.hs
- Nth Root Table: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Table/NthRoot.hs

**Translation**
- Number Theory Using Sage. This would be a great book to translate from sage to haskell.

**Data Structures**
- Simple sort generator
- Simple pre/in/post order tree generator

**Crypto**
- Create a visual cipher wheel function (because they look cool)
+ Create a frequency list: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Crypto/Frequency.hs (this is weak)
- Simple Substitution Cipher solver (experimental)

**Pearls**
- "tell" version of SmallestFreeInteger: list, array, divide & conquer,

**Other**
- Brain graph: tie components together via a graph structure, ie: gcd -> NumberTheory, gcd -> Algebra .. etc 
- Brain CPU: figure out a way to measure 'performance' on specific tasks that equate to a 'cpu architecture' (hehe). ie, ability to perform calculations of more variables (temp vars) as stored in registers -> the more registers, the more capacity. floating point vs general purpose registers. ie, my floating point register is weak and of small precision. concurrency, parallelism? Not only can we measure, but we need to identify methods for improving the architecture.
