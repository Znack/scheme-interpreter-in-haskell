module Main where

import Repl (runOne, runRepl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    _ -> runOne args
