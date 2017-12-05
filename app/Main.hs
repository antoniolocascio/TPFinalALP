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
import PDFMaker (makePDF)

main :: IO ()
main =  do 
          filepathImg <- prompt "Image: "
          filepathDoc <- prompt "Document: "
          estr <- scanImage filepathImg
          docText <- readFile filepathDoc
          case parseDoc filepathDoc docText of
             Left e    -> print e
             Right doc -> case eval doc estr of 
                           Left e -> putStrLn $ "Error: " ++ e
                           Right r -> print $ flattenResult r 


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

pdf :: IO ()
pdf = do
        docText <- readFile "test.txt"
        case parseDoc "test.txt" docText of
             Left e    -> print e
             Right doc -> makePDF doc "test.pdf"