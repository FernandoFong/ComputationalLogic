module Sesion1 where

f :: Int -> Int
f x = x + 2

cuadrado :: Int -> Int
cuadrado x = x * x

numeroMes :: Int -> String
numeroMes n
  | n == 1 = "Enero"
  | n == 2 = "Febrero"
  | n == 3 = "Marzo"
  | otherwise = "Numero Invalido"

minimo :: Int -> Int -> Int
minimo x y = if x <= y then x else y

areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c =
  let s = (a+b+c)/2 in
    sqrt(s*(s-a)*(s-b)*(s-c))

areaHeron2 :: Float -> Float -> Float -> Float
areaHeron2 a b c = sqrt(s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2
