module BubbleSort where 


    
bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
    let ln = length input

    xs <- mapM newIORef input

    forM_ [0..ln - 1] $ \_ -> do
        forM_ [0..ln - 2] $ \j -> do
            let ix = xs !! j
            let iy = xs !! (j + 1)

            x <- readIORef ix
            y <- readIORef iy

            when (x > y) $ do
                writeIORef ix y
                writeIORef iy x

    mapM readIORef xs