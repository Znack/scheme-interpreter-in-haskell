module Main where

import System.Environment
import LangParser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval 
    print