module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace docl
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
docl :: TokenParser u
docl = makeTokenParser (emptyDef   { commentLine   = "#"
                                   , reservedNames = ["page", "endpage", "section", "endsection", "multiple", "option", "yes", "no"] 
                                   })
-- Funciones de parseo parciales
docParser :: Parser Document
docParser = many1 pageParser

pageParser :: Parser Page
pageParser = do reserved docl "page"
                p <- sectParser
                reserved docl "endpage"
                return p

sectParser :: Parser Page
sectParser = do reserved docl "section"
                title <- stringLiteral docl
                subsection <- subParser
                reserved docl "endsection"
                return (Section title subsection)

subParser :: Parser Subsection
subParser = do  options <- many1 optParser
                (do  allowed <- restrParser
                     return (Options allowed options)
                 <|> return (Options False options))
            <|> do  subsections <- many1 sectParser
                    return (Subs subsections)

optParser :: Parser Option
optParser = do  reserved docl "option"
                str <- stringLiteral docl
                return str

restrParser :: Parser Restriction
restrParser = do  reserved docl "multiple"
                  (do reserved docl "yes"
                      return False
                   <|> do reserved docl "no"
                          return True)


-- Función de parseo
parseDoc :: String -> Either Error Document
parseDoc str = case parse (totParser docParser) "" str of
                    Left pe -> Left $ show pe
                    Right doc -> Right doc

parser :: String -> Document
parser s = either error id (parseDoc s)










