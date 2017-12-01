module AST where

import Data.Vector as V
import GHC.Int (Int32)
import Data.List as L

data Estructura = Rectangulo [Estructura] | Circulo Bool Double | Nada 
type Punto = (Int32, Int32)
data Contorno = C Double (V.Vector Punto) (V.Vector Contorno)

instance Show Estructura where
  show (Rectangulo hijos) = "Rectangle " L.++ show hijos
  show (Circulo True _) = "Marked Circle"
  show (Circulo False _) = "Unmarked Circle"
  show Nada = "Nothing" 

instance Show Contorno where
  show (C a puntos hijos) = "C: " L.++ "A: " L.++ show a L.++ " " L.++ show puntos L.++ "\n Hijos: " L.++ show hijos 



data Document = Section Title Subsection deriving Show
data Subsection = Subs [Document] | Options Restriction [Option] deriving Show
--data Restriction = OnlyOne | Multiple deriving Show
type Restriction = Bool --True -> solo uno
type Option = String 
type Title = String
