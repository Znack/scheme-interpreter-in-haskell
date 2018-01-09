module ListValueTypes where

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal]
                 LispVal
    | Number Integer
    | String String
    | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number value) = show value
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ "." ++ show t ++ ")"

unwordsList = unwords . map showVal