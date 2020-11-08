module Exceptions where

import Control.Monad.Except (MonadError (..), runExceptT)
import ListValueTypes (IOThrowsError, ThrowsError)

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)
