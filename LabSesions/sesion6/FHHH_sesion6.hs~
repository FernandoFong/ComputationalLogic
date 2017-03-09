--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Resolución binaria           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con gráficas. Incluye funciones relacionadas con el   --
-- problema SAT.                                                              --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------
module Graficas where

-- Sinónimo para representar a los vértices. Para fines prácticos, supondremos
-- que la gráfica almacena datos de tipo entero. De esta forma, un vértice es
-- simplemente un Int.
type Vertice = Int
 
-- Sinónimo para representar las adyacencias de un vértice. Una adyacencia es 
-- una tupla formada por un vértice y la lista de vértices con los que se 
-- conecta el primero.
type Adyacencia = (Vertice, [Vertice])

-- Sinónimo para representar a las gráficas. Siguiendo la idea anterior. Una
-- gráfica es vista como una lsita de adyacencias.
type Grafica = [Adyacencia]

-- Función que dada una gráfica regresa una lista con los vértices que la 
-- conforman (Enteros).
vertices :: Grafica -> [Vertice]
vertices g = auxVer g

auxVer :: Grafica -> [Vertice]
auxVer [] = []
auxVer (x:xs) = (fst x) : (auxVer xs)

-- Función que dada una gráfica determina si es conexa o no.
esConexa :: Grafica -> Bool
esConexa g = error "Función no implementada"

-- Función que dada una gráfica determina si es completa o no.
esCompleta :: Grafica -> Bool
esCompleta g = error "Función no implementada"

-- Fución que dada una gráfica determina si contiene un camino hamiltoniano.
cicloHamiltoniano :: Grafica -> Bool
cicloHamiltoniano g = error "Función no implementada"

-- Función que dada una gráfica y un entero k, determina si contiene un clan de
-- tamaño k.
clan :: Grafica -> Int ->  Bool
clan g k = error "Función no implementada"

perteneceVertice :: Vertice -> Grafica -> Bool
perteneceVertice v g = elem v (vertices g)

ingrado :: Grafica -> Vertice -> Int
ingrado g v = if elem v (vertices g) then auxIn g v else error "No existe el vertice"

auxIn :: Grafica -> Vertice -> Int
auxIn [(u, v)] x = if (elem x v) then 1 else 0
auxIn ((u, v):xs) x = if elem x v then 1 + auxIn xs x else auxIn xs x

exgrado :: Grafica -> Vertice -> Int
exgrado g v = if elem v (vertices g) then auxEx g v else error "No existe el vertice"

auxEx :: Grafica -> Vertice -> Int
auxEx ((u, v):xs) vi = if vi == u then length v else auxEx xs vi 

vecinosSalientes ::  Grafica -> Vertice -> [Vertice]
vecinosSalientes [(u, v)] x = if x == u then v else []
vecinosSalientes ((u,v):xs) x = if x == u then v else vecinosSalientes xs x

vecinosEntrantes :: Grafica -> Vertice -> [Vertice]
vecinosEntrantes [(u, v)] x = if elem x v then [u] else []
vecinosEntrantes ((u,v):xs) x = if elem x v then u:(vecinosEntrantes xs x) else (vecinosEntrantes xs x)

numVertices :: Grafica -> Int
numVertices g = length(vertices g)

numAristas :: Grafica -> Int
numAristas [(u, v)] = length v
numAristas ((u, v):xs) = (length v) + (numAristas xs)
