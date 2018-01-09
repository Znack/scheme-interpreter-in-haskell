module Repl where

import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout