--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Resolución binaria           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje PROP y resoución binaria.                                                             --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module PROPP3 where

-- Sinónimo para representar estados
type Estado = (VarP, Booleano)

-- Gramática para contantes lógicas
data Booleano = V | F deriving (Show,Eq)

-- Gramática para variables proposicionales
data VarP = A|B|C|D|E|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|W|X|Y|Z deriving (Show, Eq)

-- Gramática para representar átomos
data Atomo = Var VarP | Cte Booleano deriving (Eq)

-- Gramática para representar a los operadores binarios.
data OpBin = Conj | Disy | Impl | Syss deriving(Eq)

-- Gramática para representar expresiones del lenguaje Prop.
data Prop = FA Atomo
          | Neg Prop
          | Op Prop OpBin Prop deriving(Eq)

-- AQUÍ HAY QUE DEFINIR LOS TIPOS
-- CLAUSULA Y LITERAL QUE RESOLVISTE EN EL LABORATORIO.

-- Hace parte de la familia Show al tipo Atomo.
instance Show Atomo where
   show (Var v) = show v
   show (Cte b) = show b

-- Hace parte de la familia Show al tipo OpBin.
instance Show OpBin where
   show (Conj) = " ∧ "
   show (Disy) = " ∨ "
   show (Impl) = " => "
   show (Syss) = " <=> "

-- Hace parte de la familia Show al tipo Prop.
instance Show Prop where
   show (FA a) = show a
   show (Neg p) = "¬(" ++ show p ++ ")"
   show (Op p o q) = "(" ++ show p ++ show o ++ show q ++ ")"


-- Función que realiza la sustitución simultánea dada una lista con las 
-- sustituciones.
sustSimult :: Prop -> [(VarP, Prop)] -> Prop
sustSimult p l = error "Función no implementada"

-- Función que regresa el valor de interpretación aplicada a una función en los
-- estados recibidos como parámetros.
interpreta :: Prop -> [Estado] -> Booleano
interpreta p l = error "Función no implementada"

-- Función que regresa la forma normal negativa de una expresión
formaNN :: Prop -> Prop
formaNN f = error "Función no implementada"

-- Función que regresa la forma normal conjuntiva de una expresión
formaNC :: Prop -> Prop
formaNC f = error "Función no implementada"

-- Función que verifica si una fórmula es tautología
esTautologia :: Prop -> Booleano
esTautologia f = error "Función no implementada"

-- Función que decide si una fórmula es satisfacible
esSatisfacible :: Prop -> Booleano
esSatisfacible f = error "Función no implementada"

-- Función que obtiene las cláusulas de una fórmula
clausulas :: Prop -> [Prop]
clausulas f = error "Función no implementada"

--------------------------------------------------------------------------------
--                MODIFICACIONES A LA PRÁCTICA 3                              --
-- Las funciones anteriores debieron implementarse en la práctica 2 y pueden  --
-- servir como auxiliares para esta práctica.                                 --
--------------------------------------------------------------------------------

-- Función que toma dos fórmulas proposicionales e indica si son equivalentes.
equivalentes :: Prop -> Prop -> Booleano
equivalentes p q = error "Función no implementada"

-- Función que dada una fórmula, la simplifica. Esta es una versión mejorada a
-- la versión de la práctica 2.
simplifica :: Prop -> Prop
simplifica p = error "Función no implementada"

-- Función que toma dos cláusulas y una literal como parámetro y regresa su 
-- resolución binaria.
resBin :: Clausula -> Clausula -> Literal -> Clausula
resBin p q = error "Función no implementada"
