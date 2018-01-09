module LangParser where

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal]
                 LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)