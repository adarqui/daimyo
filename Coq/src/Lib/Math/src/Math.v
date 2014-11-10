Fixpoint fib (n : nat) : nat :=
 match n with
 | O => O
 | S O => S O
 | S n' => fib n' + fib (n' - 1)
 end.


Fixpoint fact (n : nat) : nat :=
 match n with
 | O => S O
 | S n' => (S n') * fact n'
 end.


Definition times (n m : nat) : nat :=
 n * m.


Definition double (n : nat) : nat :=
 times n 2.

Definition add (n m : nat) : nat :=
 n + m.

Definition mul (n m : nat) : nat :=
 n * m.

Definition sumsqr (n m : nat) : nat :=
 n * n + m.

Fixpoint inductive (base : nat) (comb : nat -> nat -> nat) (n : nat) : nat :=
 match n with
 | O => base
 | S n' => (comb n (inductive base comb n'))
 end.

Definition inductive_fact (n : nat) : nat :=
 inductive 1 mul n.

Definition inductive_sum_int (n : nat) : nat :=
 inductive 0 add n.

Definition inductive_sum_sqr (n : nat) : nat :=
 inductive 0 sumsqr n.
