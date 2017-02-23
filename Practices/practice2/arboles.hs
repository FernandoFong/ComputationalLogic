--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 2: Gramáticas / Sintaxis y semántica del lenguaje PROP.           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con árboles cuyas hojas son los únicos nodos con      --
-- información.                                                               --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module ARBOLESP2 where

-- Gramática para representar a los árboles binarios
data AB a = Hoja a
          | Mkt (AB a) (AB a) deriving (Show, Eq)

-- Función que regresa el número de hojas del árbol.
nh :: AB a -> Int
nh t = nha t

--Auxiliar de la función nh para hacer la caza de patrones.
nha :: AB a -> Int
nha (Hoja a) = 1
nha (Mkt s1 s2) = nha s1 + nha s2

-- Función que regresa el número de nodos internos del árbol.
nni :: AB a -> Int
nni t = nnia t

--Auxiliar de la función nni para hacer la caza de patrones.
nnia :: AB a -> Int
nnia (Hoja a) = 0
nnia (Mkt s1 s2) = 1 + nnia s1 + nnia s2

-- Función que determina si un elemento está contenido en el árbol.
elemA :: Eq a => AB a -> a -> Bool
elemA t e = auxElem t e

--Auxiliar que determina si está en el árbol.
auxElem :: Eq a => AB a -> a -> Bool
auxElem (Hoja a) e = e == a
auxElem (Mkt s1 s2) e = (auxElem s1 e) || (auxElem s2 e)
  
-- Función que toma un árbol y regresa una lista con los elementos en la forma
-- inorder.
inorderA :: AB a -> [a]
inorderA t = auxOrder t

--Auxiliar que regresa una lista correspondiente al recorrido inorder
auxOrder :: AB a -> [a]
auxOrder (Hoja a) = [a]
auxOrder (Mkt s1 s2) = auxOrder(s1)++auxOrder(s2)

-- Función que toma un elemento y lo agrega al árbol.
agregaHoja :: AB a -> a -> AB a
agregaHoja t e = (Mkt t (Hoja e))

-- Función que dado un árbol y una función, aplica la misma a cada elemento del
-- árbol.
mapA :: AB a -> (a -> b) -> AB b
mapA t f = auxMap t f

auxMap :: AB a -> (a -> b) -> AB b
auxMap (Hoja a) f = (Hoja (f a))
auxMap (Mkt s1 s2) f = (Mkt (auxMap s1 f) (auxMap s2 f))

-- Función que regresa la profundidad del árbol.
profundidad :: AB a -> Int
profundidad t = auxProf t

auxProf :: AB a -> Int
auxProf (Hoja a) = 0
auxProf (Mkt s1 s2) = 1 + max (auxProf s1) (auxProf s2)
