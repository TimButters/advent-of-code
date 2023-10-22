import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (pack, strip, unpack)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)

toNums :: [Char] -> [Integer]
toNums = map (\c -> if c == '(' then 1 else -1)

part1 :: [Char] -> Integer
part1 input = sum $ toNums input

part2 :: [Char] -> Int
part2 input = do
  let idx = elemIndex (-1) $ scanl1 (+) $ toNums input
  fromMaybe (-1) idx + 1

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = unpack $ strip $ pack input
  print $ part1 s
  print $ part2 s
