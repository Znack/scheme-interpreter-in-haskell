module Main where

import System.Environment
import LangParser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head