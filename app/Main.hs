module Main where

import Control.Monad
import Evaluator
import Exceptions
import LangParser
import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    _ -> runOne args
