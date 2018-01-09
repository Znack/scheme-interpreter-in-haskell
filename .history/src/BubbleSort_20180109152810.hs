module BubbleSort where 

import Data.IORef
import Control.Monad

bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
    let ln = length input

    wrappedList <- newIORef input

    forM_ [0..ln - 1] $ \_ -> do
        forM_ [0..ln - 2] $ \j -> do
            xs <- readIORef wrappedList
            let x = xs !! j
            let y = xs !! (j + 1)

            when (x > y) $ do
                writeIORef 

    mapM readIORef xs
