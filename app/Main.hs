{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parser (parseDoc)
import Eval (eval, flattenResultList)
import System.IO 
import PDFMaker (makePDF)
import ImageRec (scanImage)
import AST 
import Control.Exception as E


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
-- makeDoc = undefined
makeDoc = do
            filepathDoc <- prompt "Document description file: "
            pdfName <- prompt "Document name: "
            readRes <- catchReadFile filepathDoc
            case readRes >>= parseDoc of
              Left e    -> putStrLn $ "Error: " ++ e
              Right doc -> do makePDF doc pdfName
                              putStrLn "Done!"

scan :: IO ()
scan =  do 
          input <- prompt "Image/s: "
          docPath <- prompt "Document: "

          let imgPaths = words input
          scanRes <- scanPaths imgPaths
          readRes <- catchReadFile docPath

          case safeScan scanRes readRes of
            Left e -> putStrLn $ "Error: " ++ e
            Right fr -> putStrLn $ showFlatResult fr

  where
    scanPaths :: [FilePath] -> IO (Either Error Structure)
    scanPaths [] = return (Right [])
    scanPaths (fp : fps) = do 
      scanRes <- scanImage fp
      case scanRes of
        Left e -> return (Left e)
        Right pageStr -> do
                          scanPathsRes <- scanPaths fps
                          case scanPathsRes of
                            Left e -> return (Left e)
                            Right str -> return (Right $ pageStr : str)

    safeScan :: Either Error Structure -> Either Error String -> Either Error FlatResult
    safeScan scanRes readRes = do
      struct <- scanRes
      docText <- readRes
      doc <- parseDoc docText
      results <- eval doc struct
      return $ flattenResultList results



--Stackoverflow
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

catchReadFile :: FilePath -> IO (Either Error String)
catchReadFile fp = E.catch 
  (readFile fp >>= \s -> return $ Right s) 
  (\(e :: IOException) -> return $ Left (show e))

-- TODO
printHelp :: IO ()
printHelp = do  putStrLn "Commands: "
                putStrLn "    make"
                putStrLn "    scan"
                putStrLn "    quit"