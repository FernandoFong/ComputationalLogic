module Naturales where

data Nat = Cero
         | S Nat
         
instance Show Nat where
  show n = show $ toInt n

toInt :: Nat -> Int
toInt Cero = 0
toInt (S n) = 1 + toInt n

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma n Cero = n
suma (S n) m =  S (suma n m)
