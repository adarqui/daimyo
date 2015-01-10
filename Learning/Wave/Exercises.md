**Wave Exercises**


**Misc Math**
+ Misc Number operations: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Number.hs
+ Extract digits from a number: (digits) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Number.hs


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
- Simple Long Division generator
+ Harmonic mean: (harmonic) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Mean/Harminic.hs
+ Geometric mean: (geometric) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Mean/Geometric.hs
+ Arithmetic mean: (arithmetic) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Mean/Arithmetic.hs
+ Generalized mean: (generalized, fmean) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Mean/Generalized.hs
- RepeatedDecimal to ratio
- Simple casting out nines generator
- Simple quadratic equation generator
+ NthRoot: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/NthRoot.hs
+ NthRoot Simplifier: (simplify radical) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Algebra/NthRoot.hs
- Fraction simplifier


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
+ Semiprime: (semiPrimes, isSemiPrime) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Semiprime.hs
+ SquareFree Semiprimes: (squareFree) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Semiprime.hs
+ Square Semiprimes: (squares) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Semiprime.hs
- Waring Problem. Show that every positive integer can be written asthe sum of at most four perfect squares. One for cubes also. Waring/Hilbert decomposition.
+ Wilson Theorem: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Wilson.hs
- Set of residues
+ Euler function: (phi) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Euler.hs
+ Euler Theorem: (euler) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Euler.hs
- Cancellation
- Reduction Map & Lift
- Congruence: Units, Solvability, Order of an Element
+ Integers modulo n (The ring Z/nZ of integers modulo n): (znz) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Euler.hs
- Group
- Ring
- Field
- Group of units of the ring Z/nZ
- Figure 2.1.24 in Sage/NumT: Table of triples: (n, n-1! mod n, -1 mod n)
- Chinese Remainder Theorem
+ Casting out nines: (nines) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Nines.hs
- Euler Identity:
- Perfect Numbers
- Even Perfect Numbers
+ Abc Conjecture: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Abc.hs
+ Abundant Numbers: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Abundant.hs
+ Perfect Numbers: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Perfect.hs
+ Aliquot Parts: (aliquot parts) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Aliquot.hs
- Amicable Pair
+ Aliquot Sequences: (aliquot sequence) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Aliquot.hs
+ Aliquot Perfect Numbers: (aliquot perfect) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Aliquot.hs


**Constants**
+ Represent e: (e precision) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Constants/E.hs
+ Represent pi: (pi approx) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Constants/Pi.hs
- Represent the Brun constant
- Represent C2


**Primes (NumberTheory)**
+ Additive Prime Numbers: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Additive.hs
+ Almost Prime Numbers: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Almost.hs
+ Average Prime Numbers: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Average.hs
- Annihilating Primes
- Bell Primes
+ Carol Primes: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Carol.hs
+ Centered Decagonal Primes: (decagonal) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Centered.hs
+ Centered Heptagonal Primes: (heptagonal) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Centered.hs
+ Centered Square Primes: (square) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Centered.hs
+ Centered Triangular Primes: (triangular) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Centered.hs
+ Chen Primes: (chen) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Chen.hs
+ Circular Primes: (circular) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Circular.hs
+ Cousin Primes: (cousins) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Cousin.hs
- Cuban Primes
- Cullen Primes
- Dihedral Primes
- Double Factorial Primes
- Double Mersenne Primes
- Eisenstein Primes
- Emirps
- Euclid Primes
- Even Primes
- Factorial Primes
+ Fermat Primes: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Fermat.hs
- Fibonacci Primes
- Fortunate Primes
- Gaussian Primes
- Generalized Fermat Primes base 10
- Genocchi Number Primes
- Good Primes
- Happy Primes
- Harmonic Primes
- Higgs Primes for Squares
- Highly Cototient Number Primes
- Irregular Primes
- (p, p-5) Irregular Primes
- (p, p-9) Irregular Primes
- Isolated Primes
- Kynea Primes
- Left-Truncatable Primes
- Leyland Primes
- Long Primes
- Lucas Primes
- Lucky Primes
- Markov Primes
+ Mersenne Primes: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Mersenne.hs
- Mersenne Prime Exponents
- Mills Primes
- Minimal Primes
- Motzkin Primes
- Newman-Shanks-Williams Primes
- Non-generous Primes
- Odd Primes
- Padovan Primes
- Palindromic Primes
- Palindromic Wing Primes
- Partition Primes
- Pell Primes
- Permutable Primes
- Perrin Primes
- Pierpont Primes
- Pillai Primes
- Primeval Primes
- Primorial Primes
- Proth Primes
- Pythagorean Primes
- Prime Quadruplets
- Primes of Binary Quadratic Form
- Quartan Primes
- Ramanujan Primes
- Regular Primes
- Repunit Primes
- Primes in Residue Classes
- Right-Truncatable Primes
- Safe Primes
- Self Primes in Base 10
- Sexy Primes
- Smarandache-Wellin Primes
- Solinas Primes
- Sophie Germain Primes
- Star Primes
- Stern Primes
- Super Primes
- SuperSingular Primes
- Swinging Primes
- Thabit Number Primes
- Prime Triplets
+ Twin Primes: (twins, twins even) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/NumberTheory/Prime/Twin.hs
- Two-Sided Primes
- Ulam Number Primes
- Unique Primes
- Wagstaff Primes
- Wall-Sun-Sun Primes
- Weakly Primes
- Wieferich Primes
- Wieferich Primes: Base 3
- Wieferich Primes: Base 5
- Wieferich Primes: Base 6
- Wieferich Primes: Base 7
- Wieferich Primes: Base 10
- Wieferich Primes: Base 11
- Wieferich Primes: Base 12
- Wieferich Primes: Base 13
- Wieferich Primes: Base 17
- Wieferich Primes: Base 19
- Wilson Primes
- Wolstenholmes Primes
- Woodall Primes
- Arithmetic Prime Progressions
- Prime probabilities
- Q-test, q-test probabilities
- Goldbach Conjecture
- Ternary Goldbach Conjecture
- Hardy & Littlewood Convexity Conjecture
- Prime Producing Formulae


**Geometry**
- Express propositions/axioms from C1
- Generate diagrams of triangles using sage/haskell
+ Simple pythagorean theorem generator: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Quiz/Pythagorean.hs
+ Pythagorean Theorem 'solver': https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Pythagorean.hs
+ Pythagorean Triple Generator (triples): https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Geometry/Pythagorean.hs
- Congruency tests for triangles (side-side-side), (side-angle-side), (angle-side-angle)


**Topology**
- Define a metric space


**Linear Algebra**
- Identify various fields


**Matrix**
- Various objects (strings, lists of numbers etc) stored in matrices and operations on them.
- Simple matrix generator
+ Det, Inverse on 2x2 Matrix: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Matrix.hs
- Det, Inverse on 3x3 Matrix
- Det, Inverse on NxN Matrix
- Cofactor decomposition
+ Matrix Labeler: (tag each matrix elm as a11, a12, ...) (label, label to map): https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Matrix.hs


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


**Proof**
+ KnowShow table: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Proof/KnowShow.hs
+ Example KnowShow tables: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Proof/Basic.hs


**Mental Math**
- Simple "multiply by 11" generator
- Simple "square a number ending in 5" generator


**Tables**
+ Power Table: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Math/Table/Power.hs
+ Nth Root Table: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Table/NthRoot.hs


**Translation**
- Number Theory Using Sage. This would be a great book to translate from sage to haskell.


**Data Structures**
- Simple sort generator
- Simple pre/in/post order tree generator

**List**
+ Rotations on a list: (rotations) https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/List/Pattern.hs


**Crypto**
- Create a visual cipher wheel function (because they look cool)
+ Create a frequency list: https://github.com/adarqui/Daimyo/blob/master/Haskell/src/Daimyo/Crypto/Frequency.hs (this is weak)
- Simple Substitution Cipher solver (experimental)


**Pearls**
- "tell" version of SmallestFreeInteger: list, array, divide & conquer,


**GEB**
- MU puzzle using the MIU-system (C1)


**Other**
- Brain graph: tie components together via a graph structure, ie: gcd -> NumberTheory, gcd -> Algebra .. etc 
- Brain CPU: figure out a way to measure 'performance' on specific tasks that equate to a 'cpu architecture' (hehe). ie, ability to perform calculations of more variables (temp vars) as stored in registers -> the more registers, the more capacity. floating point vs general purpose registers. ie, my floating point register is weak and of small precision. concurrency, parallelism? Not only can we measure, but we need to identify methods for improving the architecture.
