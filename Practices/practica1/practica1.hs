module Practica1 where

-- Función que encuentra la derivada f'(v) de la ecuación f(x) = ax²+bx+c. El 
-- primer argumento de la función corresponderá al valor de a, el segundo al 
-- valor de b, el tercero al valor de c y el cuarto al valor de v que es el 
-- valor de evaluación.
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c v =  (2*a*v)+b

-- Función para calcular el área de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
areaCilindro :: Float -> Float -> Float
areaCilindro d h =
  let r = d / 2 in
    2*pi*r*(r+h)

-- Función para calcular el volumen de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
volumenCilindro :: Float -> Float -> Float
volumenCilindro d h =
  let r = d / 2 in
    pi*(r**2)*h

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
  | otherwise = 0

-- Función recursiva que calcula una aproximación con un número entero a la raíz
-- cuadrada.
raizEntera :: Int -> Int
raizEntera n = if n <= 3 then 1 else auxRaizEntera 2 n

auxRaizEntera :: Int -> Int -> Int
auxRaizEntera a n =
  let s = a * a in
    if s == n then a else if s > n then a - 1 else auxRaizEntera (a+1) n

-- Función recursiva que devuelve la suma de los primeros n números naturales.
sumaNat :: Int -> Int
sumaNat n = div (n*(n+1)) 2

-- Función recursiva que devuelve la longitud de un número entero.
longitud :: Int -> Int
longitud n = auxLong 10 n

auxLong :: Int -> Int -> Int
auxLong b n = if n < b then 1 else 1 + auxLong (b*10) n

-- Función que regresa una lista con los n primeros números de tribonacci 
-- iniciando con 0, 0, 1.
tribonaccies :: Int -> [Int]
tribonaccies n = error "Función no implementada"

-- Función que dada una lista elimina los elementos duplicados adyacentes de una
-- lista dejando únicamente una aparición de cada elemento. La implementación de
-- esta función usa foldr.
elimDup :: [a] -> [a]
elimDup ls = error "Función no implementada"

-- Función que dada una función de comparación y una lista como parámetros,
-- devuelve el elemento maximal de la lista para esa función de comparación. La
-- implementación de esta función usa foldl.
maximal :: (a -> a -> a) -> [a] -> a
maximal f l = error "Función no implementada"

-- Función que regresa la reversa de una lista.
reversa :: [a] -> [a]
reversa ls = error "Función no implementada"

-- Función que devuelve una lista con los elementos que cumplen con el predicado
-- recibido como parámetro
filtra :: (a -> Bool) -> [a] -> [a]
filtra p l = error "Función no implementada"

-- Función que toma una lista como parámetro y regresa otra lista con los 
-- elementos que aparecen una única vez en la original.
unicaVez :: [a] -> [a]
unicaVez l = error "Función no implementada"

-- Función que recibe una lista y regresa una lista de pares (k, x), donde k es
-- el número de apariciones consecutivas de x en la lista recibida.
apariciones :: [a] -> [(Int, a)]
apariciones ls = error "Función no implementada"

-- Función que dada una lista de la forma [a0,a1,a2, ... , am,an,ao,ap] devuelve
-- una lista de pares cuyos elementos son (a0,ap) (a1,ao) (a2 an). Se debe 
-- asegurar que la lista recibida siemre sea de longitud par.
empareja :: [a] -> [(a,a)]
empareja ls = error "Función no implementada"


-- AGREGA AQUÍ LA DEFINICIÓN DE LAS LISTAS POR COMPRENSIÓN
