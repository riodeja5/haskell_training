main = do
    line <- getLine
    putStrLn line
    case line of
        "" -> return()
        otherwise -> main

