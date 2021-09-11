import GHC.Base

equalString :: String -> String -> String
equalString a b =
    case (eqString a b) of
        True -> "OK"
        False -> "NG"


main = do
    a <- getLine
    b <- getLine
    putStrLn (equalString a b)

