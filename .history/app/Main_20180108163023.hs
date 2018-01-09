module Main where

import System.Environment
import Control.Exception.Base
import LangParser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head