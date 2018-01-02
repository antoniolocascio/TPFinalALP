{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parser (parseDoc)
import Eval (eval, flattenResultList)
import System.IO 
import PDFMaker (makePDF)
import ImageRec (scanImage)
import AST 
import Control.Exception as E
import System.Environment


-- Hacer bien
main :: IO ()
main = do 
        args <- getArgs
        case args of
          ["make",fp,name] -> makeDoc fp name
          ("scan":fp:images) -> scan fp images
          _ -> printHelp
        

makeDoc :: FilePath -> FilePath -> IO ()
makeDoc filepathDoc pdfName = do
            readRes <- catchReadFile filepathDoc
            case readRes >>= parseDoc of
              Left e    -> putStrLn $ "Error: " ++ e
              Right doc -> makePDF doc pdfName


scan :: FilePath -> [FilePath] -> IO ()
scan docPath imgPaths=  do 
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


catchReadFile :: FilePath -> IO (Either Error String)
catchReadFile fp = E.catch 
  (readFile fp >>= \s -> return $ Right s) 
  (\(e :: IOException) -> return $ Left (show e))

-- TODO
printHelp :: IO ()
printHelp = do  putStrLn "Commands: "
                putStrLn "    make  FILEPATH  OUTPUT_NAME"
                putStrLn "    scan  DOC_DESCRIPTION_FILEPATH  IMAGE_FILEPATH/S"