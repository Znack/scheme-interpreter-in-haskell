module Main where

import System.Environment
import LangParser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    print