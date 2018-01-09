module LangParser where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import ListValueTypes

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  return $
    case first : rest of
      "#t" -> Bool True
      "#f" -> Bool False
      atom -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseList

parseListItems :: Parser LispVal
parseListItems = List <$> sepBy parseExpr spaces

parseDottedListItems :: Parser LispVal
parseDottedListItems =
  DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseList :: Parser LispVal
parseList = do
  char '('
  x <- try parseListItems <|> parseDottedListItems
  char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

