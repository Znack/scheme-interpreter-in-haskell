module LangParser where
    
import Text.ParserCombinators.Parsec
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
    
spaces :: Parser ()
spaces = skipMany1 space