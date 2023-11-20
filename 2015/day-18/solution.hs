{-# LANGUAGE MultiWayIf #-}

import Data.Maybe (mapMaybe)

parseLines :: [String] -> [(Int, Int)]
parseLines grid = concatMap (\(line, y) -> mapMaybe (\(x, c) -> if c == '#' then Just (x, y) else Nothing) line) gridIndex
  where
    gridIndex = zip (map (zip [1..]) grid) [1..]

main :: IO ()
main =
  do
    input <- readFile "test_input.txt"
    let grid = lines input
    print $ parseLines grid
