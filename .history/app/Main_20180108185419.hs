module Main where

import System.Environment
import LangParser
import Evaluator

main :: IO ()
main = do
    arg <- head <$> getArgs
    let evaled = show <$> readExpr arg >>= eval
    print $ extractValues $ trapError evaled
