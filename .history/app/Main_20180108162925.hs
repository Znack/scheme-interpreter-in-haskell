module Main where

import LangParser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head