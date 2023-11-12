import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (maximumBy)
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

getWinner :: Map String (Int, Int, Int, Int, Int) -> String
getWinner m = winner
  where
    (winner, _) = maximumBy (\(k1, (_, _, _, d1, _))  (k2, (_, _, _, d2, _)) -> d1 `compare` d2) $ Map.toList m

runReindeerInc :: Map String (Int, Int, Int, Int, Int) -> Int -> Map String (Int, Int, Int, Int, Int)
runReindeerInc reindeer t = Map.insert winner (speed, motion, rest, distance, points + 1) newReindeer
  where
    newReindeer = fmap (`runReindeerPerSecond` t) reindeer
    winner = getWinner newReindeer
    (speed, motion, rest, distance, points) = fromJust (Map.lookup winner newReindeer)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let reindeer = map processLine $ lines input
  let distances = map (`runReindeer` 2503) reindeer
  print $ maximum distances

  let reindeerMap = Map.fromList $ map (\(n, s, m, r) -> (n, (s,m,r,0, 0))) reindeer
  --print $ foldl runReindeerInc reindeerMap [1..1000]
  print $ foldl runReindeerInc reindeerMap [1..2503]
  -- Rudolph 1084
