module Main where

import System.Environment
import Control.Exception
import LangParser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head