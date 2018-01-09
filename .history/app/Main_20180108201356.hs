module Main where

import System.Environment
import LangParser
import Evaluator
import Exceptions

main :: IO ()
main = do
    arg <- head <$> getArgs
    let evaled = return $ liftM show $ readExpr (args !! 0) >>= eval
    print $ extractValue $ trapError evaled
