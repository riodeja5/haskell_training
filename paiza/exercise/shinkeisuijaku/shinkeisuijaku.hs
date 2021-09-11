import System.IO
import Control.Monad

getRecords :: IO [(Int, Int, Int, Int)]
getRecords = do
    line <- getLine
    done <- isEOF
    -- haskellのインデックスと合わせるため1を引く
    let a_1st = (read (( words line)!!0)::Int) - 1
    let b_1st = (read (( words line)!!1)::Int) - 1
    let a_2nd = (read (( words line)!!2)::Int) - 1
    let b_2nd = (read (( words line)!!3)::Int) - 1
    let record = (a_1st, b_1st, a_2nd, b_2nd)
    if done then return [record]
                    else fmap (record:) getRecords

--                カード配置 -> 捲られた記録        -> 人数-> 各プレイヤーの枚数
calcNumOfCards :: [[Int]] -> [(Int, Int, Int, Int)] -> Int -> [Int]
calcNumOfCards cards records numOfPlayers
             = integrateGetCards (checkIfGetCards cards records) numOfPlayers

--                カード配置 -> 捲られた記録         -> 各記録で取得した枚数
checkIfGetCards :: [[Int]] -> [(Int, Int, Int, Int)] -> [Int]
checkIfGetCards cards records = [0, 1, 0, 0, 1]

--    各記録で取得した枚数 -> 人数-> 各プレイヤーの枚数
integrateGetCards :: [Int] -> Int -> [Int]
integrateGetCards gotCards numOfMembers = [0, 2]

main = do
    -- H W Nの入力
    print("H W N")
    line <- getLine
    let h = read ((words line)!!0)::Int
    let w = read ((words line)!!1)::Int
    let n = read ((words line)!!2)::Int
    print("h", h)

    -- t_{H,W}の入力
    print("t_{H,W}")
    t_H_W <- replicateM h getLine-- ["1 2 3", "3 4 5"]
    print(t_H_W)
    let t_H_Ws = map (map (read::String-> Int))-- [[1,2,3], [3,4,5]]
                     (map words t_H_W)-- [["1","2","3"], ["3","4","5"]]
    print(t_H_Ws)

    -- Lの入力
    print("L")
    line <- getLine

    -- a_i b_i A_i B_iの入力
    print("a_i b_i A_i B_i")
    records <- getRecords
    print(records)

    -- 計算
    let numOfCardsEachPlayer = calcNumOfCards t_H_Ws records n
    mapM_ print numOfCardsEachPlayer

