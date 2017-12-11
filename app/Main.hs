module Main where

import Parser (parseDoc)
import Eval (eval, flattenResult)
import System.IO 
import PDFMaker (makePDF)
import ImageRec (scanImage)


-- Hacer bien
main :: IO ()
main = do 
        ln <- prompt "> "
        case ln of
          "make" -> makeDoc >> main
          "scan" -> scan >> main
          "quit" -> return ()
          _ -> printHelp >> main
        


makeDoc :: IO ()
makeDoc = do
            filepathDoc <- prompt "Document description: "
            pdfName <- prompt "Document name: "
            docText <- readFile filepathDoc
            case parseDoc filepathDoc docText of
              Left e    -> putStrLn $ "Error: " ++ (show e)
              Right doc -> do makePDF doc pdfName
                              putStrLn "Done!"

scan :: IO ()
scan =  do 
          filepathImg <- prompt "Image: "
          filepathDoc <- prompt "Document: "
          estr <- scanImage filepathImg
          docText <- readFile filepathDoc
          case parseDoc filepathDoc docText of
             Left e    -> print e
             Right doc -> case eval doc estr of 
                           Left e -> putStrLn $ "Error: " ++ e
                           Right r -> print $ flattenResult r 

--Stackoverflow
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- TODO
printHelp :: IO ()
printHelp = do  putStrLn "Commands: "
                putStrLn "    make"
                putStrLn "    scan"
                putStrLn "    quit"