module Repl where

import Control.Monad
import Evaluator
import Exceptions
import LangParser
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr =
  return $ extractValue $ trapError (show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn
