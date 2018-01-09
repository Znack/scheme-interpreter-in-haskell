module Main where

import System.Environment
import LangParser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    let evaled = show <$> readExpr (head args) >>= eval
    print $ extractValues $ trapError evaled