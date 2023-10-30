import Data.Char (chr, ord)
import Data.List (find, group, isInfixOf, nub)

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
checkAscending nums = [1, 1] `isInfixOf` zipWith (curry (\x -> snd x - fst x)) nums (tail nums)

checkForbidden :: [Int] -> Bool
checkForbidden nums
  | (ord 'i' - 97) `elem` nums = False
  | (ord 'l' - 97) `elem` nums = False
  | (ord 'o' - 97) `elem` nums = False
  | otherwise = True

checkDoubles :: [Int] -> Bool
checkDoubles nums = (length . nub $ map head $ filter (\c -> length c > 1) $ group nums) > 1

checkPassword :: [Int] -> Bool
checkPassword nums = checkAscending nums && checkForbidden nums && checkDoubles nums

findNextPassword :: [Char] -> [Char]
findNextPassword password = numsToChars newPassword
  where
    nums = charsToNums password
    newPassword = nextValidPassword nums

nextValidPassword :: [Int] -> [Int]
nextValidPassword nums = newPassword
  where
    nextPassword = incrementNums nums
    newPassword
      | checkPassword nextPassword = nextPassword
      | otherwise = nextValidPassword nextPassword

main :: IO ()
main = do
  let input = "cqjxjnds"
  let nextPassword = findNextPassword input
  print nextPassword
  print $ findNextPassword nextPassword
