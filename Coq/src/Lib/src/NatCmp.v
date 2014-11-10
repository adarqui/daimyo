Fixpoint beq_Nat (n m : Nat) : bool :=
 match n, m with
 | O, O => true
 | S n', S m' => beq_Nat n' m'
 | _, _ => false
 end.

Example test_beq_Nat1: (beq_Nat O O) = true.
Proof. reflexivity. Qed.
Example test_beq_Nat2: (beq_Nat (S O) O) = false.
Proof. reflexivity. Qed.
Example test_beq_Nat3: (beq_Nat (S O) (S O)) = true.
Proof. reflexivity. Qed.


Fixpoint ble_Nat (n m : Nat) : bool :=
 match n, m with
 | O, _ => true
 | S n', S m' => ble_Nat n' m'
 | _, _ => false
 end.

Example test_ble_Nat1: (ble_Nat O O) = true.
Proof. reflexivity. Qed.
Example test_ble_Nat2: (ble_Nat O (S O)) = true.
Proof. reflexivity. Qed.
Example test_ble_Nat3: (ble_Nat (S O) O) = false.
Proof. reflexivity. Qed.


Definition blt_Nat (n m : Nat) : bool :=
 match (beq_Nat n m) with
 | true => false
 | false => ble_Nat n m
(* | _  => ble_Nat n m *)
 end.


Example test_blt_Nat1: (blt_Nat O O) = false.
Proof. reflexivity. Qed.
Example test_blt_Nat2: (blt_Nat O (S O)) = true.
Proof. reflexivity. Qed.
Example test_blt_Nat3: (blt_Nat (S O) O) = false.
Proof. reflexivity. Qed.


Fixpoint evenb (n : Nat) : bool :=
 match n with
 | O => true
 | S O => false
 | S (S n') => evenb n'
 end.

Definition oddb (n:Nat) : bool := negb (evenb n).

Example test_oddb1: (oddb (S O)) = true.
Proof. reflexivity. Qed.
Example test_oddb2: (oddb (S (S (S (S O))))) = false.
Proof. reflexivity. Qed.
