module FHHH_sesion2 where

sumaDigitos :: Int -> Int
sumaDigitos n
  | n <= 9 = n
  | otherwise = mod n 10 + sumaDigitos (div n 10)

concatenacion :: [a] -> [a] -> [a]
concatenacion [] ys = ys
concatenacion (x:xs) ys = x:concatenacion xs ys

aplicaSumaDigitos :: [Int] -> [Int]
aplicaSumaDigitos ls = map sumaDigitos ls

multiplo5 :: [Int] -> [Int]
multiplo5 ls = filter (\x -> mod x 5 == 0) ls

disyuncion :: [Bool] -> Bool
disyuncion ls = foldr (||) False ls

conjuncion :: [Bool] -> Bool
conjuncion ls = foldl (&&) True ls

listaA :: [Int]
listaA = [x * 13 | x <- [0..5]]

listaB :: [Int]
listaB = [1..]
