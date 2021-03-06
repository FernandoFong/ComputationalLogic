--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 2: Gramáticas / Sintaxis y semántica del lenguaje PROP.           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con expresiones aritméticas. Las expresiones trabajan --
-- con números naturales. Las operaciones que se tienen son: suma, resta,     --
-- multiplicación, división y se tiene una opción para parentizar una         --
-- expresión                                                                  --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module EAP2 where

import Data.Maybe

-- Gramática para representar a los números naturales
data Nat = Cero
         | Suc Nat deriving(Eq)

-- Gramática para representar símbolos de variables
data Id = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Show,Eq)

-- Gramática para representar a las expresiones aritméticas
data EA = Var Id
        | Cte Nat
        | Sum EA EA
        | Res EA EA
        | Mul EA EA
        | Div EA EA
        | Paren EA deriving(Eq)

-- Sinónimo para representar al ambiente de evaluación
type Env = [(Id, Int)]

-- Hace parte de la familia Show al tipo Booleano.
instance Show EA where
  show exp = showEA exp

instance Show Nat where
  show n = show $ toInt n

toInt :: Nat -> Int
toInt Cero = 0
toInt (Suc n) = 1 + toInt n
  
-- Función que dada una expresión aritmética, devuelve su representación como
-- cadena.
showEA :: EA -> String
showEA s = auxShow s

auxShow :: EA -> String
auxShow (Var a) = show a
auxShow (Cte n) = show n
auxShow (Sum e1 e2) = auxShow e1 ++ " + " ++ auxShow e2
auxShow (Res e1 e2) = auxShow e1 ++ " - " ++ auxShow e2
auxShow (Mul e1 e2) = auxShow e1 ++ " * " ++ auxShow e2
auxShow (Div e1 e2) = auxShow e1 ++ " / " ++ auxShow e2
auxShow (Paren ea) = "( " ++ auxShow ea ++ " )"


-- Función que dada una expresión y un ambiente de evaluación, devuelve el 
-- resultado de evaluar dicha expresión como el entero que la representa.
evalua :: EA -> Env -> Int
evalua ea env = auxEval ea env

auxEval :: EA -> Env -> Int
auxEval ea [] = error "No se encontró la variable en el ambiente"
auxEval (Cte c) ls = toInt c
auxEval (Var a) [(b, n)] = if a == b then n else error "falla"
auxEval (Sum e1 e2) [(b, n)] = auxEval e1 [(b, n)] + auxEval e2 [(b,n)]
auxEval (Res e1 e2) [(b, n)] = auxEval e1 [(b, n)] - auxEval e2 [(b,n)]
auxEval (Mul e1 e2) [(b, n)] = auxEval e1 [(b, n)] * auxEval e2 [(b,n)]
auxEval (Div e1 e2) [(b,n)]= auxEval e1 [(b,n)] 'div' auxEval e2 [(b,n)]
auxEval (Paren ea) ls = evalp ea ls

evalp :: EA -> Env -> Int
evalp 
