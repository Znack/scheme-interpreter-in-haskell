module Exception where

import ListValueTypes

data LispError = NumArgs Integer [LispVal] | TypeMismatch String LispVal | Parser ParseError | BadSpecialForm String LispVal 