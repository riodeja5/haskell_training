minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

main :: IO ()
main = do
    print "a"

