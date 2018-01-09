module Exceptions where

import Control.Monad.Error
import ListValueTypes
import Text.Parsec

data LispError
  = NumArgs Integer
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialForm String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ show found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at  " ++ show parseErr

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
