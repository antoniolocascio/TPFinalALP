module Eval where

import AST
import Data.List

instance Show SectNum where
  show (Num n) = show n
  show (Pnt e1 e2) = (show e1) ++ "." ++ (show e2)  

initialSectNum :: SectNum
initialSectNum = Num 1

appendSN :: SectNum -> Int -> SectNum
appendSN sn n = Pnt sn (Num n)

raise :: String -> Either Error a
raise e = Left e

--data Estructura = Rectangulo [Estructura] | Circulo Bool Double | Nada 
--data Document = Section Title Subsection deriving Show
--data Subsection = Subs [Document] | Options Restriction [Option] deriving Show

eval :: Document -> Estructura -> ErrorResult

eval (Section t (Options res opts)) (Rectangulo subestr) = do 
  results <- evalRes opts subestr t
  if res && (length (filter id results) > 1) 
    then raise $ restrictionViolation t
    else return $ Ans results

eval (Section t (Subs docs)) (Rectangulo subestr) = do
  if length docs == length subestr 
    then  do results <- mapM (\(x, y) -> eval x y) (zip docs subestr)
             return $ Sect results
    else raise $ subsectionMismatch (length docs) (length subestr) t

eval (Section t _) _ = raise $ generalMismatch t

evalRes :: [Option] -> [Estructura] -> Title -> Either Error [Res]
evalRes [opt] [Circulo mked _] _ = return $ [mked]
evalRes (opt:opts) ((Circulo mked _) : strs) t = do
  results <- evalRes opts strs t
  return $ mked : results
evalRes _ _ t = raise $ "Options don't match in section " ++ t ++ "."

flattenResult :: Result -> FlatResult
flattenResult res = flattenResult' res initialSectNum
  where
    flattenResult' :: Result -> SectNum -> FlatResult
    flattenResult' (Sect subsects) sn = concat $ map (\(s, sn) -> flattenResult' s sn) (zip subsects [appendSN sn i | i <- [1..(length subsects)]])
    flattenResult' (Ans results) sn = [(sn, findIndices id results)]


-- Errores
restrictionViolation :: Title -> Error
restrictionViolation t = "More than one selected in section " ++ t ++ "."

subsectionMismatch :: Int -> Int -> Title -> Error
subsectionMismatch expected found t = "Structure doesn't match. Expected " ++ show expected ++ " substructures in section " ++
               show t ++ " but " ++ show found ++ " were found."

generalMismatch :: Title -> Error
generalMismatch t = "Structure doesn't match in section " ++ t ++ "."
