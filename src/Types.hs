module Types where

import Data.Vector as V
import GHC.Int (Int32)
import Data.List as L


-- Tipo de datos utilizado para describir la estructura de un documento escaneado
type Structure = [StructPage]
data StructPage = Rectangle  [StructPage] | Circle Bool

instance Show StructPage where
  show (Rectangle hijos) = "Rectangle " L.++ show hijos
  show (Circle True) = "Marked Circle"
  show (Circle False) = "Unmarked Circle"



-- Tipo de datos que describe un documento
type Document = [Page] 
data Page = Section Title Subsection deriving Show
data Subsection = Subs [Page] | Options Restriction [Option] deriving Show

type Restriction = Bool 
type Option = String 
type Title = String



-- Tipos de datos utilizados para simplificar operaciones de OpenCV 
type Punto = (Int32, Int32)
data Contorno = C Double (V.Vector Punto) (V.Vector Contorno)

instance Show Contorno where
  show (C a puntos hijos) = "C: " L.++ "A: " L.++ show a L.++ " " L.++ show puntos L.++ "\n Hijos: " L.++ show hijos 



-- Representacion de resultados
type Result = [PageResult]
data PageResult = Sect [PageResult] | Ans [Res] deriving Show
type Res = Bool

type FlatResult = [(SectNum, [Int])] 
type SectNum = [Int]

showFlatResult :: FlatResult -> String
showFlatResult fr = "Results:\n" L.++ showFlatResult' fr
  where
    showFlatResult' [] = ""
    showFlatResult' ((sn, res):fr) = showDottedSN sn L.++ ": " L.++ show res L.++ "\n" L.++ showFlatResult' fr
    showDottedSN = (L.intersperse '.') . L.concat . (L.map show) . L.reverse 

type Error = String

raise :: String -> Either Error a
raise e = Left e

type ErrorPageResult = Either Error PageResult
