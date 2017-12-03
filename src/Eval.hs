module Eval where

import AST
import Data.List

data Result = Sect [Result] | Ans [Res] deriving Show
type Res = Bool
type Error = String
type ErrorResult = Either Error Result
data SectNum = Num Int | Pnt SectNum SectNum 
type FlatResult = [(SectNum, [Int])]

instance Show SectNum where
  show (Num n) = show n
  show (Pnt e1 e2) = (show e1) ++ "." ++ (show e2)  

initialSectNum :: SectNum
initialSectNum = Num 1

appendSN :: SectNum -> Int -> SectNum
appendSN sn n = Pnt sn (Num n)

raise :: String -> ErrorResult
raise e = Left e

--data Estructura = Rectangulo [Estructura] | Circulo Bool Double | Nada 
--data Document = Section Title Subsection deriving Show
--data Subsection = Subs [Document] | Options Restriction [Option] deriving Show

eval :: Document -> Estructura -> ErrorResult
eval (Section t (Options res opts)) (Rectangulo subestr) = do 
  results <- evalRes opts subestr
  if res && (length (filter id results) > 1) 
    then raise "More than one selected"
    else return $ Ans results
  --return $ Sect [Ans results]
eval (Section t (Subs docs)) (Rectangulo subestr) = do
  -- Arreglar esto! Zip elimina el resto
  if length docs == length subestr 
    then  do results <- mapM (\(x, y) -> eval x y) (zip docs subestr)
             return $ Sect results
    else raise "Structure doesn't match"
eval _ _ = raise "Structure doesn't match"

evalRes :: [Option] -> [Estructura] -> Either Error [Res]
evalRes [opt] [Circulo mked _] = return $ [mked]
evalRes (opt:opts) ((Circulo mked _) : strs) = do
  results <- evalRes opts strs
  return $ mked : results
evalRes _ _ = Left "Options don't match"

flattenResult :: Result -> FlatResult
flattenResult res = flattenResult' res initialSectNum
  where
    flattenResult' :: Result -> SectNum -> FlatResult
    flattenResult' (Sect subsects) sn = concat $ map (\(s, sn) -> flattenResult' s sn) (zip subsects [appendSN sn i | i <- [1..(length subsects)]])
    flattenResult' (Ans results) sn = [(sn, findIndices id results)]







