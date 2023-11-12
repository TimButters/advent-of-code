import Data.List (maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

processLine :: String -> (String, Int, Int, Int)
processLine line = (name, speed, motion, rest)
  where
    w = words line
    name = head w
    speed = read (w !! 3) :: Int
    motion = read (w !! 6) :: Int
    rest = read (w !! 13) :: Int

runReindeer :: (String, Int, Int, Int) -> Int -> Int
runReindeer (name, speed, motion, rest) time = complete + partial
  where
    cycle = motion + rest
    complete = time `div` cycle * speed * motion
    partial = min (time `mod` cycle) motion * speed

runReindeerPerSecond :: (Int, Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int, Int)
runReindeerPerSecond (speed, motion, rest, distance, points) sec = (speed, motion, rest, distance + delta, points)
  where
    isMoving = (sec `mod` (motion + rest)) <= motion && (sec `mod` (motion + rest) /= 0)
    delta =
      if isMoving
        then speed
        else 0

getWinnerByDistance :: Map String (Int, Int, Int, Int, Int) -> String
getWinnerByDistance m = winner
  where
    (winner, _) = maximumBy (\(k1, (_, _, _, d1, _)) (k2, (_, _, _, d2, _)) -> d1 `compare` d2) $ Map.toList m

getWinnerByPoints :: Map String (Int, Int, Int, Int, Int) -> (String, Int)
getWinnerByPoints m = (winner, points)
  where
    (winner, (_, _, _, _, points)) = maximumBy (\(k1, (_, _, _, _, p1)) (k2, (_, _, _, _, p2)) -> p1 `compare` p2) $ Map.toList m

runReindeerInc :: Map String (Int, Int, Int, Int, Int) -> Int -> Map String (Int, Int, Int, Int, Int)
runReindeerInc reindeer t = Map.insert winner (speed, motion, rest, distance, points + 1) newReindeer
  where
    newReindeer = fmap (`runReindeerPerSecond` t) reindeer
    winner = getWinnerByDistance newReindeer
    (speed, motion, rest, distance, points) = fromJust (Map.lookup winner newReindeer)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let reindeer = map processLine $ lines input
  let distances = map (`runReindeer` 2503) reindeer
  print $ maximum distances

  let reindeerMap = Map.fromList $ map (\(n, s, m, r) -> (n, (s, m, r, 0, 0))) reindeer
  print $ getWinnerByPoints $ foldl runReindeerInc reindeerMap [1 .. 2503]
