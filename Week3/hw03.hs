import Data.List

everyNth :: [a] -> Int -> [a]
everyNth ls x =
    map (\x -> ls !! (x - 1)) $ filter (\y -> mod y x == 0) [1..length(ls)]

skip :: [a] -> [[a]]
skip ls = map (everyNth ls) [1..length(ls)]

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima ls = map (\(x,y,z) -> y) $ filter (\(x,y,z) -> (x < y) && (z < y)) $ zip3 ls (tail ls) (tail (tail ls))

cnt :: Eq a => a -> [a] -> Int
cnt a = length . filter (== a)

histogram :: [Integer] -> String
histogram ls =
    let rows = map (\x -> cnt x ls) [0..9]
        maxRow = maximum rows
        stars = map (\n -> take (maxRow-n) (repeat ' ') ++ take n (repeat '*') ) rows
        chart =  (transpose stars)
        sep = (take 10 (repeat '='))
        nums = ['0'..'9']
        alls = chart ++ [sep , nums]
    in (concat (intersperse "\n" alls)) ++ "\n"
