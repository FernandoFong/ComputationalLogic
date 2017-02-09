module Sesion2 where

bunnyEars2 :: Int -> Int
bunnyEars2 0 = 0
bunnyEars2 1 = 2
bunnyEars2 n
  | mod n 2 == 0 = 3 + bunnyEars2 (n-1)
  | otherwise = 2 + bunnyEars2 (n-1)

pyramid :: Int -> Int
pyramid 0 = 0
pyramid n = n + pyramid (n-1)
