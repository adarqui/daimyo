Inductive Nat : Type :=
 | O : Nat
 | S : Nat -> Nat.

Definition pred (n : Nat) : Nat :=
 match n with
 | O => O
 | S n' => n'
 end.

Definition succ (n : Nat) : Nat :=
 match n with
 | O => S O
 | n' => S n'
 end.


Fixpoint plus (n : Nat) (m : Nat) : Nat :=
 match n with
 | O => m
 | S n' => S (plus n' m)
 end.

Fixpoint plus' (n m : Nat) : Nat :=
 match n with
 | O => m
 | S n' => S (plus n' m)
 end.


Fixpoint mult (n m : Nat) : Nat :=
 match n, m with
 | O, _ => O
 | _, O => O
 | S n', _ => plus m (mult n' m)
 end.


Example test_mult1 : (mult (S O) (S (S O))) = (S (S O)).
Proof. reflexivity. Qed.
Example test_mult2 : (mult O (S O)) = O.
Proof. reflexivity. Qed.
Example test_mult3 : (mult (S O) O) = O.
Proof. reflexivity. Qed.


Fixpoint minus' (n m : Nat) : Nat :=
 match n, m with
 | O, _ => O
 | S _ , O => n
 | S n', S m' => minus' n' m'
 end.

Example test_minus1: (minus' O (S O)) = O.
Proof. reflexivity. Qed.
Example test_minus2: (minus' (S (S O)) (S O)) = S O.
Proof. reflexivity. Qed.

Fixpoint exp (base power : Nat) : Nat :=
 match power with
 | O => S O
 | S p => mult base (exp base p)
 end.
