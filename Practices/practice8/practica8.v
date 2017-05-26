Inductive booleano : Type :=
|btrue: booleano
|bfalse: booleano.

Definition and (b1 b2: booleano) : booleano :=
  match b1 with
  |bfalse => bfalse
  |_ => b2
  end.

Definition or (b1 b2: booleano) : booleano :=
  match b1 with
  |btrue => btrue
  |_ => b2
  end.

Definition impl (b1 b2: booleano) : booleano :=
  match b1 with
  |bfalse => btrue
  |_ => b2
  end.

Definition neg (b1:booleano) : booleano :=
  match b1 with
  |btrue => bfalse
  |bfalse => btrue
  end.

Theorem andI: forall (p q: booleano), p = btrue -> q = btrue -> (and p q) = btrue.
  Proof.
    induction p.
    intros.
    simpl.
    trivial.
    intros.
    simpl.
    discriminate.
  Qed.

Theorem andE1: forall(p q: booleano), and p q = btrue -> p = btrue.
  Proof.
    induction p.
    intros.
    trivial.
    intros.
    discriminate.
  Qed.

Theorem andE2: forall(p q: booleano), and p q = btrue -> q = btrue.
  Proof.
    induction q.
    intros.
    trivial.
    intros.
    destruct p.
    discriminate.
    discriminate.
  Qed.

Theorem dobleNeg: forall(p:booleano), neg(neg(p)) = p.
  Proof.
    induction p.
    intros.
    simpl.
    trivial.
    intros.
    simpl.
    trivial.
  Qed.

Theorem elimImpl: forall(p q:booleano), p = btrue -> impl p q = btrue -> q = btrue.
  Proof.
    induction q.
    intros.
    simpl.
    trivial.
    intros.
    destruct p.
    discriminate.
    discriminate.
  Qed.

Theorem modusTollens: forall(p q:booleano), impl p q = btrue -> neg q = btrue -> neg p = btrue.
  Proof.
    induction p.
    intros.
    simpl.
    destruct q.
    discriminate.
    discriminate.
    intros.
    destruct q.
    simpl.
    trivial.
    simpl.
    trivial.
  Qed.

Theorem introImpl: forall(p q:booleano), p = btrue -> q = btrue -> impl p q = btrue.
  Proof.
    induction p.
    intros.
    simpl.
    trivial.
    intros.
    simpl.
    trivial.
  Qed.

Example ejemplo1: forall(p q r:booleano), and p q = btrue -> r = btrue -> and q r = btrue.
  Proof.
    intros.
    apply andE2 in H.
    apply andI.
    trivial.
    trivial.
  Qed.

Example ejemplo2: forall(p q r:booleano), p = btrue -> neg(neg(and q r)) = btrue -> and (neg (neg p)) r = btrue.
  Proof.
    intros.
    rewrite dobleNeg in H0.
    apply andE2 in H0.
    rewrite dobleNeg.
    apply andI.
    trivial.
    trivial.
  Qed.

Example ejemplo3: forall(p q r:booleano), and (neg p) q = btrue -> impl (and (neg p) q) (or r (neg p)) = btrue -> or r (neg p) = btrue.
  Proof.
    intros.
    apply elimImpl in H0.
    trivial.
    trivial.
  Qed.

Example ejemplo4: forall(p q r:booleano), impl p (impl q r) = btrue -> p = btrue -> neg r = btrue -> neg q = btrue.
  Proof.
    intros.
    apply elimImpl in H.
    apply modusTollens in H.
    rewrite H.
    trivial.
    rewrite H1.
    trivial.
    rewrite H0.
    trivial.
  Qed.

Variable A : Type.
Variable x : A.

Theorem elimForall: forall (p:A->Prop), (forall y:A, p y) -> p x.
  Proof.
    intros.
    apply H.
  Qed.

Theorem introExist: forall (p:A -> Prop), p x -> exists (y:A), p y.
  Proof.
    intros.
    exists x.
    trivial.
  Qed.

Example ejemplo5: forall (p q :A->Prop), (forall (y:A), p y -> q y) -> (forall (y:A), p y) -> (forall (y:A), q y).
  Admitted.

Example ejemplo6: forall (p:A->Prop), (forall (y:A), p y) -> (exists (y:A), p y).
  Proof.
    intros.
    apply elimForall in H.
    apply introExist in H.
    trivial.
  Qed.

Example ejemplo7: forall (p q :A->Prop), (forall (y:A), p y -> q y) -> (exists (y:A), p y) -> (exists (y:A), q y).
  (*Proof.
    intros.
    destruct H0.
    exists x0.*)
