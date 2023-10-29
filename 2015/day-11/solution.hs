import Data.Char (chr, ord)
import Data.List (find, isInfixOf)

numsToChars :: [Int] -> [Char]
numsToChars = map (chr . (+) 97)

charsToNums :: [Char] -> [Int]
charsToNums = map (subtract 97 . ord)

incrementNums :: [Int] -> [Int]
incrementNums nums = newNums ++ [newInt]
  where
    newInt = (last nums + 1) `mod` 26
    newNums
      | newInt == 0 = incrementNums $ init nums
      | otherwise = init nums

checkAscending :: (Eq b, Num b) => [b] -> Bool
checkAscending nums = [1, 1, 1] `isInfixOf` zipWith (curry (\x -> snd x - fst x)) nums (tail nums)

checkForbidden :: [Int] -> Bool
checkForbidden nums
    | ord 'i' `elem` nums = False
    | ord 'l' `elem` nums = False
    | ord 'o' `elem` nums = False
    | otherwise = True

-- incrementString :: [Char] -> [Char]
-- incrementString password = numsToChars $ ((charsToNums password) + 1) `mod` 26

main :: IO ()
main = do
  let input = "cqjxjnds"
  -- print $ numsToChars [0, 1, 2, 3]
  print $ incrementNums [0, 1, 2, 3, 24]
  print $ incrementNums [0, 1, 2, 3, 25]

-- print $ charsToNums "abcdz"
