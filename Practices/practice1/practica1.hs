module Practica1 where

-- Función que encuentra la derivada f'(v) de la ecuación f(x) = ax²+bx+c. El 
-- primer argumento de la función corresponderá al valor de a, el segundo al 
-- valor de b, el tercero al valor de c y el cuarto al valor de v que es el 
-- valor de evaluación.
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c v =  (2*a*v) + b

-- Función para calcular el área de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
areaCilindro :: Float -> Float -> Float
areaCilindro d h = 2*pi*r*(r + h)
  where r = d / 2

-- Función para calcular el volumen de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
volumenCilindro :: Float -> Float -> Float
volumenCilindro d h =
  let r = d / 2 in
    pi*r**2*h

-- Función que recibe tres parámetros, el primero indica la operación que se va
-- a realizar con los otros dos parámetros, las posibles operaciones son:
--
-- 's' = devuelve el segundo parámetro
-- 't' = devuelve el tercer parámetro
-- 'a' = suma
-- 'r' = resta
-- 'p' = multiplicación
-- 'd' = división entera
-- 'e' = potencia (el segundo parámetro elevado al tercero)
aplicaOperacion :: Char -> Int -> Int -> Int
aplicaOperacion op l r 
  | op == 's' = l
  | op == 't' = r
  | op == 'a' = l + r
  | op == 'r' = l - r
  | op == 'p' = l * r
  | op == 'd' = div l r
  | op == 'e' = l ^ r

-- Función recursiva que calcula una aproximación con un número entero a la raíz
-- cuadrada.
raizEntera :: Int -> Int
raizEntera n = if n <= 3 then 1 else auxRaiz 2 n

auxRaiz :: Int -> Int -> Int
auxRaiz b n = if s == n then b else if s > n then b - 1
                                    else auxRaiz (b+1) n
  where s = b * b
  
-- Función recursiva que devuelve la suma de los primeros n números naturales.
sumaNat :: Int -> Int
sumaNat 0 = 0
sumaNat n = n + sumaNat (n - 1)

-- Función recursiva que devuelve la longitud de un número entero.
longitud :: Int -> Int
longitud n = if n >= 0 then auxLong 10 n else error "Solo positivos o 0"

auxLong :: Int -> Int -> Int
auxLong b n = if n < b then 1 else 1 + auxLong (b*10) n

-- Función que regresa una lista con los n primeros números de tribonacci 
-- iniciando con 0, 0, 1.
tribonaccies :: Int -> [Int]
tribonaccies n = map tribAux [0, 1.. (n-1)]

tribAux :: Int -> Int
tribAux 0 = 0
tribAux 1 = 0
tribAux 2 = 1
tribAux n = tribAux (n-3) + tribAux (n-2) + tribAux (n-1)

-- Función que dada una lista elimina los elementos duplicados adyacentes de una
-- lista dejando únicamente una aparición de cada elemento. La implementación de
-- esta función usa foldr.
elimDup :: (Eq a) => [a] -> [a]
elimDup [] = []
elimDup (x:xs) = x:elimDup([y | y <- xs, y /= x])

-- Función que regresa la reversa de una lista.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa(xs) ++ [x]

-- Función que devuelve una lista con los elementos que cumplen con el predicado
-- recibido como parámetro
filtra :: (a -> Bool) -> [a] -> [a]
filtra p [] = []
filtra p (x:xs) = if (p x) then x:(filtra p xs) else filtra p xs

-- Función que toma una lista como parámetro y regresa otra lista con los 
-- elementos que aparecen una única vez en la original.
unicaVez :: (Eq a) => [a] -> [a]
unicaVez ls = auxUnica ls

auxUnica :: (Eq a) => [a] -> [a]
auxUnica [] = []
auxUnica [x] = [x]
auxUnica (x : xs) = if (elem x xs) then auxUnica xs
                    else [x]++auxUnica xs

-- Función que recibe una lista y regresa una lista de pares (k, x), donde k es
-- el número de apariciones consecutivas de x en la lista recibida.
apariciones :: (Eq a) => [a] -> [(Int, a)]
apariciones [] = []
apariciones l@(x:xs) = let c = cuenta x l in
                         (c, x):apariciones(elim x xs)
                         where cuenta x [] = 0
                               cuenta x (y:xs) = if x == y then 1 + cuenta x xs else cuenta x xs
                               elim x [] = []
                               elim x (y:ys) = if x == y then elim x ys else y:(elim x ys)

-- Función que dada una lista de la forma [a0,a1,a2, ... , am,an,ao,ap] devuelve
-- una lista de pares cuyos elementos son (a0,ap) (a1,ao) (a2 an). Se debe 
-- asegurar que la lista recibida siemre sea de longitud par.
empareja :: [a] -> [(a,a)]
empareja ls
  | mod (length ls) 2 == 0 = auxEmp ls
  | otherwise = error "Lista impar"

auxEmp :: [a] -> [(a, a)]
auxEmp [x, y] = [(x, y)]
auxEmp ls = (head ls, last ls):auxEmp (tail (init ls))

-- AGREGA AQUÍ LA DEFINICIÓN DE LAS LISTAS POR COMPRENSIÓN

lista1 :: [Int]
lista1 = [(2 ^ x) - 1 | x <- [0..6]]

lista2 = [(x, (x+1)) | x <- [3, 6.. ]]
