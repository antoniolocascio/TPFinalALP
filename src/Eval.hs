module Eval where

import AST
import Data.List

-- type Structure = [StructPage]
-- data StructPage = Rectangle  [StructPage] | Circle Bool
-- type Document = [Page] 
-- data Page = Section Title Subsection deriving Show
-- data Subsection = Subs [Page] | Options Restriction [Option] deriving Show


eval :: Document -> Structure -> Either Error [Result]
eval [] [] = return []
eval (page:pages) (str:strs) = do 
  res <- evalPage page str
  results <- eval pages strs
  return $ res : results
eval _ _ = raise "The number of pages in the description file doesn't match with the number of pages scanned."

evalPage :: Page -> StructPage -> ErrorResult
evalPage (Section t (Options res opts)) (Rectangle subestr) = do 
  results <- evalRes opts subestr t
  if res && (length (filter id results) > 1) 
    then raise $ restrictionViolation t
    else return $ Ans results

evalPage (Section t (Subs docs)) (Rectangle subestr) = do
  if length docs == length subestr 
    then  do results <- mapM (\(x, y) -> evalPage x y) (zip docs subestr)
             return $ Sect results
    else raise $ subsectionMismatch (length docs) (length subestr) t

evalPage (Section t _) _ = raise $ generalMismatch t

evalRes :: [Option] -> [StructPage] -> Title -> Either Error [Res]
evalRes [opt] [Circle mked] _ = return $ [mked]
evalRes (opt:opts) ((Circle mked) : strs) t = do
  results <- evalRes opts strs t
  return $ mked : results
evalRes _ _ t = raise $ "Options don't match in section: " ++ t ++ "."


flattenResultList :: [Result] -> FlatResult
flattenResultList results = concat $ map (\(res, i) -> flattenResult res i) (zip results [1..(length results)])

flattenResult :: Result -> Int -> FlatResult
flattenResult res i = flattenResult' res [i]
  where
    flattenResult' :: Result -> SectNum -> FlatResult
    flattenResult' (Sect subsects) sn = concat $ map (\(s, sn) -> flattenResult' s sn) (zip subsects [i : sn | i <- [1..(length subsects)]]) 
    flattenResult' (Ans results) sn = [(sn, findIndices id results)]


-- Errores
restrictionViolation :: Title -> Error
restrictionViolation t = "More than one selected in section: " ++ t ++ "."

subsectionMismatch :: Int -> Int -> Title -> Error
subsectionMismatch expected found t = "Structure doesn't match. Expected " ++ show expected ++ " substructures in section: " ++
               show t ++ " but " ++ show found ++ " were found."

generalMismatch :: Title -> Error
generalMismatch t = "Structure doesn't match in section: " ++ t ++ "."


