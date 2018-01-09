module Exception where

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
    | NotFunction String String
    | UnboundVar String String
    | Default String
