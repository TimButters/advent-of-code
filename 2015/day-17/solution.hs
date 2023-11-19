combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

findCombinations :: [Int] -> Int -> Int -> [[Int]]
findCombinations tubs target size = filter ((target==).sum) $ combinations size tubs

main :: IO ()
main =
  do
    input <- readFile "input.txt"
    let s = map read $ lines input :: [Int]
    let combos =  concatMap (findCombinations s 150) [1..length s]
    print $ length combos
    print $ map (\n -> (n, length $ filter ((n==).length) combos)) [1..10]
