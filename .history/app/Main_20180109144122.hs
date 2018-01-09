module Main where

import Control.Monad
import Evaluator
import Exceptions
import LangParser
import System.Environment
import Repl

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"

parseAndEval :: String -> String
parseAndEval expr = do
  let evaled = fmap show $ readExpr expr >>= eval
  extractValue $ trapError evaled
