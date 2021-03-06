module LangParser where

import Control.Monad.Except (MonadError (throwError))
import ListValueTypes
  ( LispError (Parser),
    LispVal (Atom, Bool, DottedList, List, Number, String),
    ThrowsError,
  )
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr =
  readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

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
