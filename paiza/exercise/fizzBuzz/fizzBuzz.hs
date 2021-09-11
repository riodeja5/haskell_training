import GHC.Base

toFizzBuzz :: Int -> String
toFizzBuzz x = 
    if (x `mod` 15) == 0 then "Fizz Buzz"
    else if (x `mod` 3) == 0 then "Fizz"
    else if (x `mod` 5) == 0 then "Buzz"
    else show x

toFizzBuzzes :: [Int] -> [String]
toFizzBuzzes nums = map toFizzBuzz nums

main = do
    line <- getLine
    let max = read line::Int
    let fizzBuzzes = toFizzBuzzes [1..max]
    mapM_ putStrLn fizzBuzzes

