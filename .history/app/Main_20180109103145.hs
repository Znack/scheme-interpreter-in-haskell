module Main where

import System.Environment
import Control.Monad
import LangParser
import Evaluator
import Exceptions

main :: IO ()
main = do
    arg <- head <$> getArgs
    print $ parseAndEval arg

parseAndEval expr = do
    let evaled = fmap show $ readExpr arg >>= eval
    extractValue $ trapError evaled