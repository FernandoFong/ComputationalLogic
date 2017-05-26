--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 4: Relación de alpha-equivalencia y Algoritmo de Martelli         --
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

consT :: Termino -> [Nombre]
consT (V n) = []
consT (F n []) = [n]
consT (F n l) = consTAux l

consTAux :: [Termino] -> [Nombre]
consTAux [] = []
consTAux ((V _):xs) = consTAux xs
consTAux ((F n []):xs) = n:consTAux xs
consTAux ((F n ls):xs) = consTAux ls ++ consTAux xs

varT :: Termino -> [Nombre]
varT (V n) = [n]
varT (F n []) = []
varT (F n l) = varAux l

varAux :: [Termino] -> [Nombre]
varAux [] = []
varAux ((V n):xs) = n:varAux xs
varAux ((F n []):xs) = varAux xs
varAux ((F n l):xs) = varAux l ++ varAux xs

funT :: Termino -> [Nombre]
funT (V n) = []
funT (F n []) = []
funT (F n l) = n:auxFun l

auxFun :: [Termino] -> [Nombre]
auxFun [] = []
auxFun ((V _):xs) = auxFun xs
auxFun ((F n []):xs) = auxFun xs
auxFun ((F n ls):xs) = n:((auxFun ls)++(auxFun xs))

consF :: FORM -> [Nombre]
consF TrueF = []
consF FalseF = []
consF (Pr n [V _]) = []
consF (Pr n [F a []]) = [a]
consF (Pr n [F a ls]) = consTAux ls
consF (Eq (V a) (V b)) = []
consF (Eq (F n xs) (F x ls)) = consTAux xs++consTAux ls
consF (Neg f) = consF f
consF (Disy p q) = consF p ++ consF q
consF (Impl p q) = consF p ++ consF q
consF (Equi p q) = consF p ++ consF q
consF (PT n p) = consF p
consF (EX n p) = consF p

varF :: FORM -> [Nombre]
varF TrueF = []
varF FalseF = []
varF (Pr n ((V a):xs)) = a:(varAux xs)
varF (Pr n ((F a []):xs)) = varAux xs
varF (Pr n ((F a ls):xs)) = varAux (ls++xs)
varF (Eq (F n xs) (F x ls)) = varAux xs++varAux ls
varF (Neg f) = varF f
varF (Disy p q) = varF p ++ varF q
varF (Impl p q) = varF p ++ varF q
varF (Equi p q) = varF p ++ varF q
varF (PT n p) = varF p
varF (EX n p) = varF p

funF :: FORM -> [Nombre]
funF TrueF = []
funF FalseF = []
funF (Pr n [V a]) = []
funF (Pr n [F a []]) = []
funF (Pr n [F a ls]) = a:auxFun ls
funF (Eq (F n xs) (F x ls)) = auxFun xs++auxFun ls
funF (Neg f) = funF f
funF (Conj p q) = funF p ++ funF q
funF (Disy p q) = funF p ++ funF q
funF (Impl p q) = funF p ++ funF q
funF (Equi p q) = funF p ++ funF q
funF (PT n p) = funF p
funF (EX n p) = funF p

subT :: Termino -> [Termino]
subT (V n) = [(V n)]
subT (F n []) = [(F n [])]
subT (F n l) = [(F n [])]++auxSubT l

auxSubT :: [Termino] -> [Termino]
auxSubT [] = []
auxSubT ((V n):xs) = (V n):(auxSubT xs)
auxSubT ((F a ls):xs) = (F a []):((auxSubT ls)++(auxSubT xs))

subF :: FORM -> [FORM]
subF TrueF = [TrueF]
subF FalseF = [FalseF]
subF (Pr n [V a]) = [(Pr n [V a])]
subF (Pr n ((V a):xs)) = (Pr n [V a]):auxSubF xs
subF (Pr n [F a ls]) = (Pr n [F a []]):auxSubF ls
subF (Eq p q) = auxSubF [p] ++ auxSubF [q]
subF (Neg f) = (Neg f):subF f
subF (Conj p q) = (Conj p q):((subF p) ++ (subF q))
subF (Disy p q) = (Disy p q):((subF p) ++ (subF q))
subF (Impl p q) = (Impl p q):((subF p) ++ (subF q))
subF (Equi p q) = (Equi p q):((subF p) ++ (subF q)) 
subF (PT n f) = (PT n f):(subF f)
subF (EX n f) = (EX n f):(subF f)

auxSubF :: [Termino] -> [FORM]
auxSubF [] = []
auxSubF [(V n)] = [(Pr [] [(V n)])]
auxSubF ((V n):xs) = (Pr [] [(V n)]):(auxSubF xs)
auxSubF ((F a ls):xs) = (Pr [] [F a []]):((auxSubF ls)++(auxSubF xs))

cuantF :: FORM -> [FORM]
cuantF TrueF = []
cuantF FalseF = []
cuantF (Pr p ls) = []
cuantF (PT n p) = p:(cuantF p)
cuantF (EX n p) = p:(cuantF p)
cuantF (Neg f) = cuantF f
cuantF (Conj p q) = cuantF p ++ cuantF q
cuantF (Disy p q) = cuantF p ++ cuantF q
cuantF (Impl p q) = cuantF p ++ cuantF q
cuantF (Equi p q) = cuantF p ++ cuantF q

fv :: FORM -> [Nombre]
fv TrueF = []
fv FalseF = []
fv (Pr p ls) = varAux ls
fv (Eq p q) = varAux [p, q]
fv (PT x f) = elimina x (varF f)
fv (EX x f) = elimina x (varF f)
fv (Neg f) = fv f
fv (Conj p q) = (fv p) ++ (fv q)
fv (Disy p q) = (fv p) ++ (fv q)
fv (Impl p q) = (fv p) ++ (fv q)
fv (Equi p q) = (fv p) ++ (fv q)

elimina :: Nombre -> [Nombre] -> [Nombre]
elimina n [] = []
elimina n (x:xs) = if (n == x) then (elimina n xs) else x:(elimina n xs)

-- Función que verifica si dos fórmulas son alpha-equivalentes.
vAlfaEq :: FORM -> FORM -> Bool
vAlfaEq = error "funcion no implementada"
  
-- Función que renombra las variables ligadas de una fórmula de manera que las
-- listas de variables libres y ligadas que sean ajenas. Estos es un casa
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
simpSus [] = []
simpSus ((n, (V m)):xs) = if n == m then simpSus xs else (n,(V m)):(simpSus xs)
simpSus (x:xs) = x:(simpSus xs)

-- Función que dadas dos sustituciones devuelve su composición.
compSus :: Sust -> Sust -> Sust
compSus s t = s++t

-- Función que dados dos términos devuelve una lista de sustituciones de tal
-- forma que:
-- · Si t1, t2 no son unificables la lista es vacía.
-- · Si sí lo son, la lista contiene como único elemento al unificador 
--   correspondiente.
unifica :: Termino -> Termino -> [Sust]
unifica (V x) (V y) = if x == y then [] else [[(x, (V y))]]
unifica (V x) (F f ls)
  | (elem x (varAux ls)) = []
  | otherwise = [[(x, (F f ls))]]
unifica (F f ls) (V x) = unifica (V x) (F f ls)
unifica (F f fs) (F g gs)
  | not(f == g) || not((length fs) == (length gs)) = error "FALLA"
  | otherwise = []

-- Función que implementa el caso general para unificar un conjunto (lista)
-- W = {t1,..,tn}
unificaConj :: [Termino] -> [Sust]
unificaConj ls = error "Función no implementada"


