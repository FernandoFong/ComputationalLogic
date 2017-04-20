--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Relación de alpha-equivalencia y Algoritmo de Martelli         --
-- Montanari                                                                  --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje FORM y unificación                                                --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------
module PrimerOrden where

-- Sinónimo para representar los nombres de variables y funciones.
type Nombre = String

-- Sinónimo para representar sustituciones.
type Sust = [(Nombre,Termino)]

-- Gramática para representar términos.
data Termino = V Nombre
             | F Nombre [Termino] deriving (Eq)

-- Gramática para representar fórmulas de la Lógica de Primer Orden.
data FORM = TrueF
             | FalseF
             | Pr Nombre [Termino]
             | Eq Termino Termino
             | Neg FORM
             | Conj FORM FORM
             | Disy FORM FORM
             | Impl FORM FORM
             | Equi FORM FORM
             | PT Nombre FORM
             | EX Nombre FORM deriving(Eq)

instance Show Termino where
  show (V n) = n
  show (F n []) = n
  show (F n (x:xs)) = n++"("++(show x) ++(auxShowT xs)++")"

instance Show FORM where
  show (TrueF) = "T"
  show (FalseF) = "F"
  show (Pr n ls) = n++"("++auxShowT ls++")"
  show (Eq t1 t2) = "("++show t1 ++ "==" ++ show t2++")"
  show (Neg f) = "¬("++show f ++")"
  show (Conj f1 f2) = "("++show f1 ++"/\\"++ show f2++")"
  show (Disy f1 f2) = "("++show f1 ++"\\/"++ show f2++")"
  show (Impl f1 f2) = "("++show f1 ++"=> "++ show f2++")"
  show (Equi f1 f2) = "("++show f1 ++"<=>"++ show f2++")"
  show (PT n f) = "V"++n++ show f
  show (EX n f) = "Э"++n++show f

auxShowT :: [Termino] -> String
auxShowT [] = ""
auxShowT (x:xs) = show x++auxShowT xs

-- Función que verifica si dos fórmulas son alpha-equivalentes.
vAlfaEq :: FORM -> FORM -> Bool
vAlfaEq f g = error "Función no implementada"

-- Función que renombra las variables ligadas de una fórmula de manera que las
-- listas de variables libres y ligadas que sean ajenas. Estos es un caso 
-- particular de la siguiente función.
renVL :: FORM -> FORM
renVL f = error "Función no implementada"

-- Función que renombra la variables ligadas de una fórmula de forma que sus 
-- nombres sean ajenos a los de una lista dada.
renVLconj :: FORM -> [Nombre] -> FORM
renVLconj f ls = error "Función no implementada"

-- Función que implementa la sustitución en fórmulas usano la 
-- alpha-equivalencia.
apsubF2 :: FORM -> Sust -> FORM
apsubF2 f s = error "Función no implementada"

-- Función que dada una sustitución, elimina de ella los pares con componentes
-- iguales correspondientes a sustituciones de la forma x:=x.
simpSus :: Sust -> Sust
simpSus s = error "Función no implementada"

-- Función que dadas dos sustituciones devuelve su composición.
compSus :: Sust -> Sust -> Sust
compSus s t = error "Función no implementada"

-- Función que dados dos términos devuelve una lista de sustituciones de tal
-- forma que:
-- · Si t1, t2 no son unificables la lista es vacía.
-- · Si sí lo son, la lista contiene como único elemento al unificador 
--   correspondiente.
unifica :: Termino -> Termino -> [Sust]
unifica t s = error "Función no implementada"

-- Función que implementa el caso general para unificar un conjunto (lista)
-- W = {t1,..,tn}
unificaConj :: [Termino] -> [Sust]
unificaConj ls = error "Función no implementada"

apsubT :: Termino -> Sust -> Termino
apsubT t [] = t 
apsubT (V n) ((x,t):xs)= if n == x then t else apsubT (V n) xs
apsubT (F n []) ls = (F n [])
apsubT (F n l) s = (F n (map (\x -> apsubT x s) l))

apsubF :: FORM -> Sust -> FORM
apsubF TrueF _ = TrueF
apsubF FalseF _ = FalseF
apsubF (Pr n ls) s = (Pr n (map (\x-> apsubT x s) ls))
apsubF (Eq t1 t2) s = (Eq (apsubT t1 s) (apsubT t2 s))
apsubF (Neg f) s= (Neg (apsubF f s))
apsubF (Conj f1 f2) s = (Conj (apsubF f1 s) (apsubF f2 s))
apsubF (Disy f1 f2) s = (Disy (apsubF f1 s) (apsubF f2 s))
apsubF (Impl f1 f2) s = (Impl (apsubF f1 s) (apsubF f2 s))
apsubF (Equi f1 f2) s = (Equi (apsubF f1 s) (apsubF f2 s))
apsubF (PT n f) s
  | not (elem n (nomSust s) || elem n (termSust s)) = PT n (apsubF f s)
  | otherwise = (PT n f)
apsubF (EX n f) s
  | not (elem n (nomSust s) || elem n (termSust s)) = EX n (apsubF f s)
  | otherwise = (EX n f)

nomSust :: Sust -> [Nombre]
nomSust [] = []
nomSust ((x,t):xs) = x:(nomSust xs)

termSust :: Sust -> [Nombre]
termSust [] = []
termSust ((x,(V n)):xs) = n:(termSust xs)
termSust ((x,(F a l)):xs) = a:((auxTermS l)++(termSust xs))

auxTermS :: [Termino] -> [Nombre]
auxTermS [] = []
auxTermS ((V n):xs) = n:(auxTermS xs)
auxTermS ((F a []):xs) = a:(auxTermS xs)
auxTermS ((F a ls):xs) = a:((auxTermS ls)++(auxTermS xs))

varT :: Termino -> [Nombre]
varT (V n) = [n]
varT (F n []) = []
varT (F n l) = varAux l

varAux :: [Termino] -> [Nombre]
varAux [] = []
varAux ((V n):xs) = n:varAux xs
varAux ((F n []):xs) = varAux xs
varAux ((F n l):xs) = varAux l ++ varAux xs
