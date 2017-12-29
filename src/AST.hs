module AST where

import Data.Vector as V
import GHC.Int (Int32)
import Data.List as L

--data Estructura = Rectangulo [Estructura] | Circulo Bool Double | Nada 
type Structure = [StructPage]
data StructPage = Rectangle  [StructPage] | Circle Bool

type Punto = (Int32, Int32)
data Contorno = C Double (V.Vector Punto) (V.Vector Contorno)

instance Show StructPage where
  show (Rectangle hijos) = "Rectangle " L.++ show hijos
  show (Circle True) = "Marked Circle"
  show (Circle False) = "Unmarked Circle"
  --show Nada = "Nothing" 

instance Show Contorno where
  show (C a puntos hijos) = "C: " L.++ "A: " L.++ show a L.++ " " L.++ show puntos L.++ "\n Hijos: " L.++ show hijos 



-- data Document = Section Title Subsection deriving Show
-- data Subsection = Subs [Document] | Options Restriction [Option] deriving Show


type Document = [Page] 
data Page = Section Title Subsection deriving Show
data Subsection = Subs [Page] | Options Restriction [Option] deriving Show

type Restriction = Bool --True -> solo uno
type Option = String 
type Title = String

data Result = Sect [Result] | Ans [Res] deriving Show
type Res = Bool

type Error = String
type ErrorResult = Either Error Result


type SectNum = [Int]
type FlatResult = [(SectNum, [Int])] 

showFlatResult :: FlatResult -> String
showFlatResult fr = "Results:\n" L.++ showFlatResult' fr
  where
    showFlatResult' [] = ""
    showFlatResult' ((sn, res):fr) = "Question " L.++ showDottedSN sn L.++ ": " L.++ show res L.++ "\n" L.++ showFlatResult' fr
    showDottedSN = (L.intersperse '.') . L.concat . (L.map show) . L.reverse 