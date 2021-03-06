module Repl where

import Control.Monad (liftM)
import Evaluator (bindVars, eval, primitiveBindings)
import Exceptions (liftThrows, runIOThrows)
import LangParser (readExpr)
import ListValueTypes (Env, LispVal (Atom, List, String))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl =
  primitiveBindings
    >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
  env <-
    primitiveBindings
      >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr
