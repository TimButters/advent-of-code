import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Text (pack, strip, unpack)

strToList :: [Char] -> [[[Char]]]
strToList input = map (splitOn "x") (splitOn "\n" input)

strToIntList :: [[String]] -> [[Int]]
strToIntList = map (map (read :: String -> Int))

parseInput :: [Char] -> [[Int]]
parseInput s = strToIntList $ strToList s

minTwo :: Ord a => [a] -> [a]
minTwo xs = take 2 $ sort xs

wrappingPaper :: [Int] -> Int
wrappingPaper (x : y : z : _) = 2 * a1 + 2 * a2 + 2 * a3 + product (minTwo [x, y, z])
  where
    a1 = x * y
    a2 = x * z
    a3 = y * z

bows :: (Num a, Ord a) => [a] -> a
bows xs = product xs + sum (map (2 *) (minTwo xs))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = parseInput $ unpack $ strip $ pack input
  print $ sum $ map wrappingPaper s
  print $ sum $ map bows s
