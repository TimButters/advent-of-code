import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Text (pack, splitOn, strip, unpack)

parseLines :: [String] -> Map (String, String) Int
parseLines lines = Map.fromList $ concatMap parseLine lines

parseLine :: String -> [((String, String), Int)]
parseLine line = [((start, end), distance), ((end, start), distance)]
  where
    elements = words line
    start = head elements
    end = elements !! 2
    distance = (read . last) elements :: Int

minDistance :: [(String, Int)] -> (String, Int)
minDistance = minimumBy (comparing snd)

findPath :: Map String [(String, Int)] -> [(String, Int)] -> [(String, Int)]
findPath cities [] = [(start, 0)]
  where
    start = fst . head $ Map.toList cities
findPath cities path = newPath ++ findPath cities newPath
  where
    (next, dist) = minDistance $ fromJust $ Map.lookup ((fst . last) path) cities
    newPath = path ++ [(next, dist)]

getAllStops :: Map (String, String) Int -> [String]
getAllStops stopMap = nub $ map fst $ Map.keys stopMap

pathDistance :: [String] -> Map (String, String) Int -> Int
pathDistance [x] _ = 0
pathDistance path distances = fromJust (Map.lookup (listToTuple (take 2 path)) distances) + nextStop
  where
    nextStop = pathDistance (drop 1 path) distances

listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  let m = parseLines s
  let stops = getAllStops m
  let paths = permutations stops
  let pathLengths = map (`pathDistance` m) paths
  print $ "Part 1: " ++ show (minimum pathLengths)
  print $ "Part 2: " ++ show (maximum pathLengths)
