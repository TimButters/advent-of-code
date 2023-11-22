{-# LANGUAGE MultiWayIf #-}

import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

parseLines :: [String] -> Set (Int, Int)
parseLines grid = Set.fromList $ concatMap (\(line, y) -> mapMaybe (\(x, c) -> if c == '#' then Just (x, y) else Nothing) line) gridIndex
  where
    gridIndex = zip (map (zip [1 ..]) grid) [1 ..]

surroundingPoints :: (Int, Int) -> [(Int, Int)]
surroundingPoints (cx, cy) = [(x, y) | x <- [cx - 1 .. cx + 1], y <- [cy - 1 .. cy + 1], (x, y) /= (cx, cy)]

staysOn :: (Int, Int) -> Set (Int, Int) -> Bool
staysOn coord lights = numLightsOn == 2 || numLightsOn == 3
  where
    numLightsOn = length $ filter id $ map (`Set.member` lights) (surroundingPoints coord)

turnsOn :: (Int, Int) -> Set (Int, Int) -> Bool
turnsOn coord lights = numLightsOn == 3
  where
    numLightsOn = length $ filter id $ map (`Set.member` lights) (surroundingPoints coord)

checkPosition :: (Int, Int) -> Set (Int, Int) -> Bool
checkPosition coord lights = newState
  where
    isOn = Set.member coord lights
    newState
      | isOn = staysOn coord lights
      | otherwise = turnsOn coord lights

runLights :: [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
runLights grid lights = Set.fromList $ catMaybes $ zipWith (\coord on -> if on then Just coord else Nothing) grid lightToggles
  where
    lightToggles = map (`checkPosition` lights) grid

runLights2 :: Int -> Int -> [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
runLights2 width height grid lights = Set.fromList $ catMaybes $ zipWith (\coord on -> if on then Just coord else Nothing) grid lightToggles
  where
    lightToggles_ = map (`checkPosition` lights) grid
    lightToggles =
      zipWith
        ( \light coord ->
            coord == (1, 1)
              || coord == (width, 1)
              || coord == (1, height)
              || coord == (width, height)
              || light
        )
        lightToggles_
        grid

main :: IO ()
main =
  do
    input <- readFile "input.txt"
    let textGrid = lines input
    let width = length $ head textGrid
    let height = length textGrid
    let grid = [(x, y) | x <- [1 .. width], y <- [1 .. height]]
    let lights = parseLines textGrid
    print $ length $ iterate (runLights grid) lights !! 100
    print $ length $ iterate (runLights2 width height grid) lights !! 100
