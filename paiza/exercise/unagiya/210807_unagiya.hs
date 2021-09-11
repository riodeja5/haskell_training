import System.IO
import Data.List.Extra

makeSeats :: Int -> Int -> Int -> [Int]
makeSeats numOfSeats numOfGuests seatNo =
    if (seatNo + numOfGuests - 1 > numOfSeats) then [seatNo..numOfSeats] ++ [1..(seatNo+numOfGuests-1 -numOfSeats)]
                                                else [seatNo..(seatNo+numOfGuests-1)]

-- currentリストとnewリストのマージ結果を返す。ただしcurrentにnewの一部が含まれていたら、マージせずcurrentを返す。
mergeSeats :: [Int] -> [Int] -> [Int]
mergeSeats current new
    | (length new) <= 0 = current
    -- TODO この条件だけだとダメなことが判明した
    | elem (head new) current = current
    | elem (last new) current = current
    | otherwise = current ++ new

-- 座席を確保する。第一引数に全座席数、第二引数に(座席数, 確保開始の座席番号)リスト
-- 戻り値に、確保成功した座席番号リスト
takeSeats :: Int -> [(Int, Int)] -> [Int]
takeSeats numOfSeats inputs
    | (length inputs) <= 0 = []
    | otherwise = mergeSeats (takeSeats numOfSeats (init inputs))
                                (makeSeats numOfSeats (fst (last inputs)) (snd (last inputs)))

getInputs :: IO [(Int, Int)]
getInputs = do
    line <- getLine
    done <- isEOF
    let numOfGuests = read ((words line)!!0)::Int
    let seatNo = read ((words line)!!1)::Int
    let input = (numOfGuests, seatNo)
    if done then return [input]
                     else fmap (input:) getInputs

main = do
    -- 考え方としては、グループでループして、可能な座席番号を取っていき、取得した座席番号リストを作る。
    -- リストのサイズが取得できた座席数（解）
    line <- getLine
    let numOfSeats = read ((words line)!!0)::Int
    let numOfGroups = read ((words line)!!1)::Int

    inputs <- getInputs 
--    print inputs-- for debug

    let seats = takeSeats numOfSeats inputs
--    print seats-- for debug
    print (length seats)

