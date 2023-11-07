import Data.List (nub, permutations)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T

preProcessLine :: String -> String
preProcessLine s = T.unpack $ T.strip $ T.map (\c -> if c == '.' then ' ' else c) $ T.pack s

parseLine :: String -> ((String, String), Int)
parseLine line = ((name1, name2), score)
  where
    wds = words line
    name1 = head wds
    name2 = last wds
    score =
      if wds !! 2 == "lose"
        then -1 * read (wds !! 3) :: Int
        else read (wds !! 3) :: Int

buildMap :: [String] -> Map (String, String) Int
buildMap s = Map.fromList $ map parseLine s

buildMapWithMe :: [String] -> Map (String, String) Int
buildMapWithMe s = m `Map.union` newM
  where
    m = Map.fromList $ map parseLine s
    people = nub $ map fst $ Map.keys m
    newM = Map.fromList $ concat [[(("me", p), 0), ((p, "me"), 0)] | p <- people]

generateAllSeatings :: Map (String, String) Int -> [[String]]
generateAllSeatings m = permutations people
  where
    people = nub $ map fst $ Map.keys m

seatingPairsHelper :: [String] -> [(String, String)]
seatingPairsHelper [x] = []
seatingPairsHelper (x : y : xs) = (x, y) : seatingPairsHelper (y : xs)

seatingPairs :: [String] -> [(String, String)]
seatingPairs xs = seatingPairsHelper xs ++ [(last xs, head xs)]

scoreSeating :: [(String, String)] -> Map (String, String) Int -> Int
scoreSeating seating m = sum $ map (\(a, b) -> fromJust (Map.lookup (a, b) m) + fromJust (Map.lookup (b, a) m)) seating

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = map preProcessLine $ lines input
  let m = buildMap s
  let seating = generateAllSeatings m
  print $ maximum $ map (flip scoreSeating m . seatingPairs) seating

  let mWithMe = buildMapWithMe s
  let seatingWithMe = generateAllSeatings mWithMe
  print $ maximum $ map (flip scoreSeating mWithMe . seatingPairs) seatingWithMe
