module ListValueTypes where

import Control.Monad.Error
import Data.IORef
import System.IO
import Text.Parsec

type Env = IORef [(String, IORef LispVal)]

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Port Handle
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Func { fparams :: [String]
         , fvararg :: Maybe String
         , fbody :: [LispVal]
         , fclosure :: Env }

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number value) = show value
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
  show (Port _) = "<IO port>"
  show (PrimitiveFunc _) = "<primitive>"
  show (IOFunc _) = "<IO primitive>"
  show Func{fparams = args, fvararg = varargs} =
    "(lambda (" ++
    unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg) ++
    ") ...)"

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
