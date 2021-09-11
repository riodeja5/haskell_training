import System.IO

getInts::IO [Int]

getInts = do
    line <- getLine
    done <- isEOF
    let num = read line::Int
    if done then return [num]
                      else fmap (num:) getInts

main = do
    ints <- getInts
    let min = minimum ints
    print min

