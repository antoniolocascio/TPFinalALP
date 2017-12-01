module Eval where

import AST

data Result = Sect Result | [Res] deriving Show
data Res = Marked | Unmarked deriving Show