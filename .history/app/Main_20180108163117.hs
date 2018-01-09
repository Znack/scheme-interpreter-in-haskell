module Main where

import System.Environment
import LangParser
import Evaluator

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
