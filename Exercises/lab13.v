(* Definición inductiva de naturales *)
Inductive natural: Type :=
|cero : natural
|succ : natural -> natural.

(* Suma en naturales *)
Fixpoint add (n1 n2:natural) : natural :=
  match n2 with
  | cero => n1
  | succ n2' => succ (add n1 n2')
  end.

(* Multiplicación en naturales *)
Fixpoint mul (n1 n2:natural) :natural :=
  match n2 with
  | cero => cero
  | succ n2' => add (mul n1 n2') n1
  end.


(* AQUÍ VAN SUS PRUEBAS *)

Lemma cero_neutro_izq: forall (n:natural), add cero n = n.
Proof.
  induction n.
  simpl.
  trivial.
  intros.
  simpl.
  rewrite IHn.
  simpl.
  trivial.
Qed.

Lemma suc_mas_1: forall (n:natural), add n (succ cero) = succ(n).
Proof.
  induction n.
  simpl.
  trivial.
  intros.
  simpl.
  trivial.
Qed.

Lemma suc_mas_1_der: forall(n:natural), add (succ cero) n = succ(n).
Proof.
  induction n.
  simpl.
  trivial.
  intros.
  simpl.
  rewrite IHn.
  trivial.
Qed.

Lemma cero_absorb_izq: forall (n:natural), mul cero n = cero.
Proof.
  induction n.
  simpl.
  trivial.
  intros.
  simpl.
  trivial.
Qed.

Lemma sum_suc: forall (a b:natural), add (succ a) b = succ(add a b).
Admitted.

Lemma add_conm: forall (n1 n2:natural), add n1 n2 = add n2 n1.
Proof.
  induction n1.
  intros.
  simpl.
  rewrite cero_neutro_izq.
  trivial.
  intros.
  simpl.
  rewrite sum_suc.
  rewrite IHn1.
  simpl.
  trivial.
Qed.

Lemma add_assoc: forall (a b c:natural), add (add a b) c = add a (add b c).
Proof.
  induction a.
  intros.
  simpl.
  rewrite cero_neutro_izq.
  rewrite cero_neutro_izq.
  simpl.
  trivial.
  intros.
  rewrite sum_suc.
  rewrite sum_suc.
  rewrite IHa.
  rewrite sum_suc.
  trivial.
Qed.