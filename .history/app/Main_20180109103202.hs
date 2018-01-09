module Main where

import Control.Monad
import Evaluator
import Exceptions
import LangParser
import System.Environment

main :: IO ()
main = do
  arg <- head <$> getArgs
  print $ parseAndEval arg

parseAndEval :: String -> String
parseAndEval expr = do
  let evaled = fmap show $ readExpr expr >>= eval
  extractValue $ trapError evaled
