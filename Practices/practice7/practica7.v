Section stack.

  (* Definición de stack *)
  Inductive stack: Type :=
  | empty: stack
  | push: nat -> stack -> stack.

  (* Definición de la operación pop que toma un stack y lo devuelve sin el elemento del tope *)
  Definition pop (s:stack) : stack :=
    match s with
    | empty => empty
    | push _ s' => s'
    end.

  (* Función que nos regresa el elemento al tope de stack si existe y None en otro caso *)
  Definition top (s:stack) : (option nat) :=
    match s with
    | empty => None
    | push n _ => Some n
    end.

  (* Concatenación de stacks *)
  Fixpoint concatenate (s1 s2: stack) : stack :=
    match s1 with
    | empty => s2
    | push n s1' => push n (concatenate s1' s2)
    end.

  (* Notación para poder usar ++ en la concatenación *)
  Notation " x ++ y " := (concatenate x y).

  (* Devuelve la reversa de un stack *)
  Fixpoint reverse (s: stack) : stack :=
    match s with
    | empty => empty
    | push n s' => (reverse s') ++ (push n empty)
    end.

  (* AQUÍ VAN SUS PRUEBAS *)
  Example ejercicio1: pop(pop(pop(empty))) = empty.
  Proof.
    simpl.
    trivial.
  Qed.

  Example ejercicio2: forall x:nat, pop(push x (pop (push x empty))) = empty.
  Proof.
    intros.
    simpl.
    trivial.
  Qed.

  Proposition ejercicio3: forall (s:stack) (x:nat) , top s = Some x -> s <> empty.
  Proof.
    induction s.
    simpl.
    intros.
    discriminate.
    intros.
    discriminate.
  Qed.

  Theorem ejercicio4: forall (s:stack) (x:nat), pop (push x s) = s.
  Proof.
    intros.
    simpl.
    trivial.
  Qed.

  Lemma ejercicio5: forall s:stack, s ++ empty = s.
  Proof.
    induction s.
    simpl.
    trivial.
    intros.
    simpl.
    rewrite IHs.
    trivial.
  Qed.

  Theorem ejercicio6: forall n:nat, reverse(push n empty) = push n empty.
  Proof.
    intros.
    simpl.
    trivial.
  Qed.

  Lemma ejercicio7: forall s1 s2 s3:stack, (s1 ++ s2) ++ s3 = s1 ++ (s2 ++ s3).
  Proof.
    induction s1.
    intros.
    simpl.
    trivial.
    intros.
    simpl.
    rewrite IHs1.
    trivial.
  Qed.

  Theorem ejercicio8: forall s1 s2:stack, reverse (concatenate s1 s2) = concatenate (reverse s2) (reverse
                                                                                                s1).
  Proof.
    induction s1.
    intros.
    simpl.
    rewrite ejercicio5.
    trivial.
    intros.
    simpl.
    rewrite IHs1.
    rewrite ejercicio7. 
    trivial.
  Qed.
End stack.