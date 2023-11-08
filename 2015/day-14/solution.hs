import GHC.CmmToAsm.Reg.Linear.State (runR)
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

main :: IO ()
main = do
  input <- readFile "input.txt"
  let reindeer = map processLine $ lines input
  let distances = map (`runReindeer` 2503) reindeer
  print $ maximum distances
