/*Predicado que dada una lista, nos regresa su último elemento o nos dice si dado un elemento, es el último de la lista.*/
ultimo([X], X).
ultimo([_|T], X) :- ultimo(T, X).

/*Dada una lista y un entero, nos dice si el entero corresponde a la lista o qué entero debemos de pasarle con esa lista, para que se evalúe a
true*/
longitud([], 0).
longitud([_|T], X) :- longitud(T,Y) -> X is Y+1.

/*Dada una lista, recorre cada elemento un lugar a la izquierda.*/
recorrei([], []).
recorrei([H|T], X) :- concatena(T, [H], X).

/*Concatena dos listas dadas.*/
concatena([],L,L).
concatena([H|T],L2,[H|L3]) :- concatena(T,L2,L3).

/*Dada una lista, recorre cada elemento un lugar a la derecha.*/
recorred([], []).
recorred(L, X) :- ultimo(L,Y), concatena([Y], init(L), X).

/*AFN que acepta al lenguaje que no tienen 2 b's seguidas y que empiezan con b. */
/*Declaramos cuáles son estados.*/
estado(1).
estado(2).
estado(3).

/*Declaramos la función de transición.*/
delta(1, b, 2).
delta(2, a, 2).
delta(2, a, 3).
delta(3, b, 2).

/*Declaramos los estados finales.*/
final(3).

/*Esto es equivalente a que el autómata vaya leyendo símbolos de la cadena de entrada.*/
acepta([H|T]) :- auxacepta([H|T], 1).

/*Auxiliar para poder simular las transiciones del autómata.*/
auxacepta([], X) :- final(X).
auxacepta([H|T], X) :- delta(X, H, Y), auxacepta(T, Y).

palindromo(A) :- palindromo(A, []).
palindromo(A, A).
palindromo([_|A], A).
palindromo([C|A], D) :- palindromo(A, B), B=[C|D].
