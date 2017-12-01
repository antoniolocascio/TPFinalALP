module Parser where

import Text.ParserCombinators.Parsec
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
docl = makeTokenParser (emptyDef   { commentStart  = "/*"
                                   , commentEnd    = "*/"
                                   , commentLine   = "//"
                                   , reservedNames = ["section", "multiple", "option", "yes", "no"]
                                   , reservedOpNames = [":",";"]
                                   })

docParser :: Parser Document
docParser = do  reserved docl "section"
                reservedOp docl ":"
                title <- stringLiteral docl
                reservedOp docl ";"
                subsection <- subParser
                return (Section title subsection)

subParser :: Parser Subsection
subParser = do  options <- many1 optParser
                (do  reserved docl "multiple"
                     reservedOp docl ":"
                     allowed <- restrParser
                     reservedOp docl ";"
                     return (Options allowed options)
                 <|> return (Options False options))
            <|> do  subsections <- many1 docParser
                    return (Subs subsections)

optParser :: Parser Option
optParser = do  reserved docl "option"
                reservedOp docl ":"
                str <- stringLiteral docl
                reservedOp docl ";"
                return str

restrParser :: Parser Restriction
restrParser = do  reserved docl "multiple"
                  reservedOp docl ":"
                  do reserved docl "yes"
                     return False
                  <|> do reserved docl "no"
                         return True


------------------------------------
-- Función de parseo
------------------------------------
parseDoc :: SourceName -> String -> Either ParseError Document
parseDoc = parse (totParser docParser)

parser :: String -> Document
parser s = either (error. show) id (parseDoc "" s)











