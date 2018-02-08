{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parser (parseDoc)
import Eval (eval, flattenResultList)
import System.IO 
import PDFMaker (makePDF)
import ImageRec (scanImage)
import Types
import Control.Exception as E
import System.Environment

-- Parsea las dos opciones posibles de comandos
main :: IO ()
main = do 
        args <- getArgs
        case args of
          ["make",fp,name] -> makeDoc fp name
          ("scan":fp:images) -> scan fp images
          _ -> printHelp
        

-- Crear un archivo PDF en base a la descripcion de un documento
makeDoc :: FilePath -> FilePath -> IO ()
makeDoc filepathDoc pdfName = do
            readRes <- catchReadFile filepathDoc
            case readRes >>= parseDoc of
              Left e    -> putStrLn $ "Error: " ++ e
              Right doc -> makePDF doc pdfName


-- Escanear imagenes y compararlas con una descripcion de un documento
scan :: FilePath -> [FilePath] -> IO ()
scan docPath imgPaths=  do 
          scanRes <- scanPaths imgPaths
          readRes <- catchReadFile docPath

          case safeParseEval scanRes readRes of
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

    safeParseEval :: Either Error Structure -> Either Error String -> Either Error FlatResult
    safeParseEval scanRes readRes = do
      struct <- scanRes
      docText <- readRes
      doc <- parseDoc docText
      results <- eval doc struct
      return $ flattenResultList results


catchReadFile :: FilePath -> IO (Either Error String)
catchReadFile fp = E.catch 
  (readFile fp >>= \s -> return $ Right s) 
  (\(e :: IOException) -> return $ Left (show e))

printHelp :: IO ()
printHelp = do  putStrLn "Commands: "
                putStrLn "    make  FILEPATH  OUTPUT_NAME"
                putStrLn "    scan  DOC_DESCRIPTION_FILEPATH  IMAGE_FILEPATH(S)"