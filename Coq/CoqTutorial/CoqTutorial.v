(* https://coq.inria.fr/V8.1/tutorial.html *)
(* https://coq.inria.fr/refman/tactic-index.html *)
(* https://coq.inria.fr/refman/Reference-Manual011.html#@tactic3 *)
(* https://coq.inria.fr/library/Coq.Logic.Classical_Prop.html *)

Check 0.
Check O.

(* Declarations *)
Section Declaration.

(* let n be a natural number *)
Variable n : nat.

(* let n be a positive natural number *)
Hypothesis Pos_n : (gt n 0).

Check (nat -> Prop).

Check gt n O.
Check gt n.
Check gt.

(* Definitions *)
Section Definitions.

Definition one := (S O).
Definition  two : nat := S one.

Definition double (m : nat) := plus m m.
Definition add_n (m : nat) := plus m n.

Check (forall m:nat, gt m 0).

(* Minimal Logic *)
Section MinimalLogic.

Variables A B C : Prop.

Check (A -> B).

Check (A -> Set).
Check (A -> Prop).
Check (A -> Type).

(* We want to prove the easy tautology ((A -> (B -> C)) -> (A -> B) -> (A -> C) *)
Goal (A -> B -> C) -> (A -> B) -> A -> C.
Goal (A -> (B -> C)) -> (A -> B) -> (A -> C).
    intro H.
    intros H' HA.
    apply H.
    exact HA.
    apply H'.
    assumption.
    Save trivial_lemma.

Lemma distr_impl : (A -> B -> C) -> (A -> B) -> A -> C.
    intros.
    apply H; [ assumption | apply H0; assumption ].
    Save.

Lemma distr_imp : (A -> B -> C) -> (A -> B) -> A -> C.
    auto.
    Save.

(* Propoisitional Calculus *)
Abort.
Section PropositionalCalculus. 

Lemma and_Commutative : A /\ B -> B /\ A.
    intro H.
    elim H.
    auto.
    Qed.

Check conj.
Check (forall A B : Prop, A -> B -> A /\ B).
Check ((A : Prop) -> (B : Prop) -> (A : Prop) /\ (B : Prop)).

Goal (A /\ B -> B /\ A).
    Abort.

Lemma or_commutative : A \/ B -> B \/ A.
    intro H; elim H.
    intro HA.
    clear H.
    right.
    trivial.
    Abort.

Lemma or_commutative' : A \/ B -> B \/ A.
    tauto.
    Qed.

Print or_commutative'.

Lemma distr_and : A -> B /\ C -> (A -> B) /\ (A -> C).
    tauto.
    Qed.

Print distr_and.

Lemma Peirce : ((A -> B) -> A) -> A.
    try tauto.
    Abort.

Lemma NNPeirce : ~~ (((A -> B) -> A) -> A).
    tauto.
    Qed.


(* Example *)
Section Club.

(*
Here is one more example of propositional reasoning, in the shape of a Scottish puzzle. A private club has the following rules:
Every non-scottish member wears red socks
Every member wears a kilt or doesn't wear red socks
The married members don't go out on Sunday
A member goes out on Sunday if and only if he is Scottish
Every member who wears a kilt is Scottish and married
Every scottish member wears a kilt
Now, we show that these rules are so strict that no one can be accepted.
*)


Variables Scottish RedSocks WearKilt Married GoOutSunday : Prop.

Hypothesis rule1 : ~Scottish -> RedSocks.
Hypothesis rule2 : WearKilt \/ ~RedSocks.
Hypothesis rule3 : Married -> ~GoOutSunday.
Hypothesis rule4 : GoOutSunday <-> Scottish.
Hypothesis rule5 : WearKilt -> Scottish /\ Married.
Hypothesis rule6 : Scottish -> WearKilt.

Lemma NoMember : False.
    tauto.
    Qed.

End Club.

Check NoMember.

Reset Initial.


Section PredicateCalculus.
Variable D : Set.
Variable R : D -> D -> Prop.

Section R_sym_trans.
Hypothesis R_symmetric: forall x y : D, R x y -> R y x.
Hypothesis R_transitive : forall x y z : D, R x y -> R y z -> R x z.

(* Remark the syntax forall x:D, which stands for universal quantification ∀ x : D. *)

Lemma refl_if : forall x:D, (exists y, R x y) -> R x x.
    intros x x_Rlinked.
    elim x_Rlinked.
    intros y Rxy.
    apply R_transitive with y.
    assumption.
    apply R_symmetric.
    assumption.
    Qed.

End R_sym_trans.


Variable P : D -> Prop.
Variable d : D.

Lemma weird : (forall x:D, P x) -> exists a, P a.
    intro UnivP.
    exists d.
    trivial.
    Qed.

(*
drinkers' paradox: “In any non-empty bar, there is a person such that if she drinks, then everyone drinks”. *)
Hypothesis EM : forall A : Prop, A \/ ~A.
Lemma drinker : exists x:D, P x -> forall x:D, P x.
    elim (EM (exists x, ~P x)).
    intro Non_drinker; elim Non_drinker; intros Tom Tom_does_not_drink.
    exists Tom; intro Tom_drinks.
    absurd (P Tom); trivial.
    intro No_nondrinker; exists d; intro d_drinks.
    intro Dick; elim (EM (P Dick)); trivial.
    intro Dick_does_not_drink; absurd (exists x, ~P x); trivial.
    exists Dick.
    trivial.
    Qed.

End PredicateCalculus.

Check refl_if.
Check weird.
Check drinker.


(* Equality *)
Section Equality.

Variable f : nat -> nat.
Hypothesis foo : f 0 = 0.
Lemma L1 : forall k:nat, k = 0 -> f k = k.
    intros k E.
    rewrite E.
    apply foo.
    Qed.

Hypothesis f10 : f 1 = f 0.
Lemma L2 : f (f 1) = 0.
    replace (f 1) with 0.
    apply foo.
    transitivity (f 0).
    symmetry.
    trivial.
    replace (f 0) with 0.
    rewrite f10; rewrite foo; trivial.
    Qed.



Reset Initial.

(* C2, Induction *)

Inductive bool : Set := true | false.
Check bool.
Check bool_rect.
Check bool_ind.
Check bool_rec.

Lemma duality : forall b:bool, b = true \/ b = false.
    intros b.
    elim b.
    left; trivial.
    right; trivial.
    Restart.
    simple induction b; auto.
    Qed.

Inductive nat : Set :=
    | O : nat
    | S : nat -> nat.

Definition prim_rec := nat_rec (fun i:nat => nat).
Check prim_rec.
Eval cbv beta in
        (fun _ : nat => nat) O ->
        (forall n : nat, (fun _ : nat => nat) n -> (fun _ : nat => nat) (S n)) ->
        forall n : nat, (fun _ : nat => nat) n.

Definition addition (n m:nat) := prim_rec m (fun p rec:nat => S rec) n.
Eval compute in (addition (S (S O)) (S (S (S O)))).

(* Using Fixpoint *)
Fixpoint plus (n m:nat) {struct n} : nat :=
    match n with
    | O => m
    | S p => S (plus p m)
    end.

Reset bool.

Lemma plus_n_O : forall n:nat, n = n + 0.
    intro n.
    elim n.
    simpl.
    auto.
    simpl; auto.
    Qed.

Hint Resolve plus_n_O.

Lemma plus_n_S : forall n m:nat, S (n + m) = n + S m.
    simple induction n; simpl; auto.
    Qed.

Lemma plus_com : forall n m:nat, n + m = m + n.
    simple induction m; simpl; auto.
    intros m' E; rewrite <- E; auto.
    Qed.


(* Discriminate *)
Definition Is_S (n:nat) :=
    match n with
    | O => False
    | S p => True
    end.

Lemma S_Is_S : forall n:nat, Is_S (S n).
    simpl; trivial.
    Qed.

(* Logic Programming *)

Inductive le (n:nat) : nat -> Prop :=
    | le_n : le n n
    | le_S : forall m:nat, le n m -> le n (S m).

Check le.
Check le_ind.
