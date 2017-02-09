module Vectores where

type Vector = (Double, Double, Double)

suma :: Vector -> Vector -> Vector
suma (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

productoK :: Double -> Vector -> Vector
productoK d (x, y, z) = (d*x, d*y, d*z)

productoP :: Vector -> Vector -> Double
productoP (x1,y1,z1) (x2, y2, z2) = (x1*x2) + (y1*y2) + (z1*z2)

norma ::Vector -> Double
norma (x,y,z) = sqrt((x ** 2) + (y ** 2) + (z ** 2))
