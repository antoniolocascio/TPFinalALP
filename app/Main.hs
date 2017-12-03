{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}


import TestMain (scanImage)
import Parser (parseDoc)
import Eval (eval, flattenResult)
import System.IO 

main :: IO ()
main =  do 
          filepathImg <- prompt "Image: "
          filepathDoc <- prompt "Document: "
          estr <- scanImage filepathImg
          docText <- readFile "test.txt"
          case parseDoc filepathDoc docText of
             Left e    -> print e
             Right doc -> case eval doc estr of 
                           Left e -> print e
                           Right r -> print $ flattenResult r 


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine