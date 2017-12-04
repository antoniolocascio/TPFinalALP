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
  results <- evalRes opts subestr t
  if res && (length (filter id results) > 1) 
    then raise $ "More than one selected in section " ++ t ++ "."
    else return $ Ans results
eval (Section t (Subs docs)) (Rectangulo subestr) = do
  if length docs == length subestr 
    then  do results <- mapM (\(x, y) -> eval x y) (zip docs subestr)
             return $ Sect results
    else raise $ "Structure doesn't match. Expected " ++ show (length docs) ++ " substructures in section " ++
               show t ++ " but " ++ show (length subestr) ++ " were found."
eval (Section t _) _ = raise $ "Structure doesn't match in section " ++ t ++ "."

evalRes :: [Option] -> [Estructura] -> Title -> Either Error [Res]
evalRes [opt] [Circulo mked _] _ = return $ [mked]
evalRes (opt:opts) ((Circulo mked _) : strs) t = do
  results <- evalRes opts strs t
  return $ mked : results
evalRes _ _ t = Left $ "Options don't match in section " ++ t ++ "."

flattenResult :: Result -> FlatResult
flattenResult res = flattenResult' res initialSectNum
  where
    flattenResult' :: Result -> SectNum -> FlatResult
    flattenResult' (Sect subsects) sn = concat $ map (\(s, sn) -> flattenResult' s sn) (zip subsects [appendSN sn i | i <- [1..(length subsects)]])
    flattenResult' (Ans results) sn = [(sn, findIndices id results)]





