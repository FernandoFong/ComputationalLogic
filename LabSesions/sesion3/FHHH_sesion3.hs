module FHHH_sesion3 where

data Nat = Cero
         | S Nat

instance Show Nat where
  show n = show $ toInt n

toInt :: Nat -> Int
toInt Cero = 0
toInt (S n) = 1 + toInt n

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (S n) m =  S (suma n m)

producto :: Nat -> Nat -> Nat
producto n Cero = Cero
producto n (S Cero) = n
producto n (S m) = suma n (producto n m)

potencia :: Nat -> Nat -> Nat
potencia n Cero = (S Cero)
potencia n (S Cero) = n
potencia (S n)(S m) = potencia n (potencia n m)

