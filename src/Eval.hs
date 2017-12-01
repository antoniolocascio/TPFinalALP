module Eval where

import AST
--import Data.List as L

data Result = Sect [Result] | Ans [Res] deriving Show
data Res = Marked | Unmarked deriving Show

type Error = String
type ErrorResult = Either Error Result

raise :: String -> ErrorResult
raise e = Left e

--data Estructura = Rectangulo [Estructura] | Circulo Bool Double | Nada 
--data Document = Section Title Subsection deriving Show
--data Subsection = Subs [Document] | Options Restriction [Option] deriving Show

eval :: Document -> Estructura -> ErrorResult
eval (Section t (Options res opts)) (Rectangulo subestr) = do 
  results <- evalRes opts subestr
  return $ Sect [Ans results]
eval (Section t (Subs docs)) (Rectangulo subestr) = do
  results <- mapM (\(x, y) -> eval x y) (zip docs subestr)
  return $ Sect results
eval _ _ = raise "Structure doesn't match"

evalRes :: [Option] -> [Estructura] -> Either Error [Res]
evalRes [opt] [Circulo mked _] = return $ [toRes mked]
evalRes (opt:opts) ((Circulo mked _) : strs) = do
  results <- evalRes opts strs
  return $ (toRes mked) : results
evalRes _ _ = Left "Options don't match"

toRes :: Bool -> Res
toRes True  = Marked
toRes False = Unmarked 