Require Import SLC.Lib.
Require Import SLC.Booleans.
Require Import Coq.Logic.Classical.
Require Import stdpp.set.

Axiom classicT: forall P: Prop, P + ~P.

Definition propInBool (P: Prop): bool :=
 if (classicT P)
 then true
 else false.

Notation "'B@(' p ')'" := (propInBool p) (at level 80).


(* Hint Unfold propInBool. *)

Definition set_subset {T: Type} (a b: set T): bool :=
  propInBool ((set_difference a b) ≡ set_empty).

Lemma Aeq_dec_all: forall T: Type, forall x y: T, {x = y} + {x <> y}.
  intros.
  pose proof classicT (x  = y) as H.
  destruct H; intuition.
Qed.


Lemma trueProp: forall P, propInBool P = true <-> P.
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma falseProp: forall P, propInBool P = false <-> (P -> False).
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma falseNegProp: forall P, negb (propInBool P) = false <-> P.
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma trueNegProp: forall P, negb (propInBool P) = true <-> (P -> False).
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma trueProp2: forall P, true = propInBool P <-> P.
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma falseProp2: forall P, false = propInBool P <-> (P -> False).
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Lemma falseNegProp2: forall P, false = negb (propInBool P) <-> P.
Proof.
  repeat libStep || unfold propInBool in *  || ifthenelse_step.
Qed.

Lemma trueNegProp2: forall P, true = negb (propInBool P) <-> (P -> False).
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.


Lemma equivProps: forall P1 P2,
    propInBool P1 = propInBool P2 <->
    (P1 <-> P2).
Proof.
  repeat libStep || unfold propInBool in * || ifthenelse_step.
Qed.

Hint Rewrite trueProp falseProp falseNegProp trueNegProp equivProps: libR.
Hint Rewrite trueProp2 falseProp2 falseNegProp2 trueNegProp2: libR.

Lemma equal_booleans: forall b1 b2: bool,
    (b1 = true -> b2 = true) ->
    (b2 = true -> b1 = true) ->
    b1 = b2.
Proof.
  destruct b1; destruct b2; repeat libStep.
Qed.

Ltac t_propbool :=
  match goal with
  (*| H: true = propInBool ?P |- _ =>
    poseNew (Mark P "trueProp2");
    pose proof (proj1 (trueProp2 _) H)
  | H: propInBool ?P = true |- _ =>
    poseNew (Mark P "trueProp");
    pose proof (proj1 (trueProp _) H)
  | H: false = propInBool ?P |- _ =>
    poseNew (Mark P "falseProp2");
    pose proof (proj1 (falseProp2 _) H)
  | H: propInBool ?P = false |- _ =>
    poseNew (Mark P "falseProp");
    pose proof (proj1 (falseProp _) H)*)
  | H: propInBool ?P = ?b |- _ =>
    let pib := fresh "pib" in
    not_literal b;  
    destruct b eqn:pib
  | H: ?b = propInBool ?P |- _ =>
    let pib := fresh "pib" in
    not_literal b;
    destruct b eqn:pib
  end.