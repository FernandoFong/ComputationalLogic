/*Dado un mapa con seis países arbitrario. Escribir una regla colorea(P1,P2,P3,P4,P5,P6) en
 Prolog que coloree el mapa usando únicamente cuatro colores de manera que dos países adyacentes
no tengan el mismo color*/

color(azul).
color(rojo).
color(amarillo).
color(verde).

adyacente(Pais1, Pais2) :- color(Pais1), color(Pais2), Pais1 \= Pais2.

colorea(P1, P2, P3, P4, P5, P6) :- adyacente(P1, P2), adyacente(P1, P3), adyacente(P1, P4), adyacente(P1, P5),
                                 adyacente(P2, P3), adyacente(P2, P4), adyacente(P2, P5),
                                 adyacente(P3, P4), adyacente(P3, P5),
                                 adyacente(P5, P6).

/* Definir el comportamiento de entrada y salida de las compuertas lógicas AND y NOT. */
and(0, 0 ,0).
and(1, 0 ,0).
and(0, 1 ,0).
and(1, 1, 1).

not(0, 1).
not(1, 0).

/* A partir de lo anterior, describir la relación entre la entrada y salida de los siguientes circuitos: */
%Circuito A:
circuitoA(X,Y,Z) :- and(X,Y,Result), not(Result,Z).

%CircuitoB:
circuitoB(V, Z, Y, X, W) :- and(V, Z, Result1), and(Y, X, Result2),
                            and(Result1, Result2, Result3), not(Result3, W).

/* Representamos el predicado ser árbol binario en Prolog, de la siguiente forma: */
bt(void).
bt(node(A,T1,T2)) :- integer(A), bt(T1), bt(T2).
/* Dado esto, implementar las siguientes reglas: */

elem(A, node(B,_,_)) :- A == B.
elem(A, node(_,T1,_)) :- elem(A,T1).
elem(A, node(_,_,T2)) :- elem(A,T2).

maxtree(_, void).
maxtree(A, node(B,T1,T2)) :- A > B, maxtree(A,T1), maxtree(A,T2).

mintree(_,void).
mintree(A, node(B,T1,T2)) :- A < B, mintree(A,T1), mintree(A,T2).

innode(A,T) :- elem(A,T).

/*El predicado ser árbol binario de búsqueda se representa de la siguiente manera*/

bst(void).
bst(node(A,T1,T2)) :- integer(A),maxtree(A,T1),
		      mintree(A,T2),bst(T1),bst(T2).

elembst(A, node(B,_,_)) :- A == B.
elembst(A, node(B,T1,_)) :- (A < B) -> elembst(A,T1).
elembst(A, node(B,_,T2)) :- (A > B) -> elembst(A,T2).

insert(A, void, node(A, void, void)).
insert(A, T, T) :- T = node(A, _, _).
insert(A, node(B, T1, T2), node(B, NuevoArbol, T2)) :- A < B, insert(A, T1, NuevoArbol).
insert(A, node(B, T1, T2),  node(B, T1, NuevoArbol)) :- A > B, insert(A, T2, NuevoArbol).

