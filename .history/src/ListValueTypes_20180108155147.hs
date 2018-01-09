module ListValueTypes where

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal]
                 LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number value) = show value
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
