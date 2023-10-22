{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Map qualified as Map
import Data.Map.Strict (Map, empty)
import Data.Maybe (Maybe, fromJust, fromMaybe, isNothing, maybe)
import Data.Text (pack, strip, unpack)
import Text.RE.PCRE (countMatches, matches, re, (*=~), (=~))

getCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getCoords start end = [(x, y) | x <- [xStart .. xEnd], y <- [yStart .. yEnd]]
  where
    xStart = fst start
    yStart = snd start
    xEnd = fst end
    yEnd = snd end

turnOn :: [(Int, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
turnOn coords lights = foldl (\m c -> Map.insertWith (+) c 1 m) lights coords

turnOff :: [(Int, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
turnOff coords lights = foldr (Map.alter (\c -> if isNothing c then Nothing else Just (max (fromJust c - 1) 0))) lights coords

toggle :: [(Int, Int)] -> Map (Int, Int) Int -> Map (Int, Int) Int
toggle coords lights = foldl (\m c -> Map.insertWith (+) c 2 m) lights coords

lightCommand :: String -> (Int, Int) -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
lightCommand command start end lights
  | command == "on" = turnOn (getCoords start end) lights
  | command == "off" = turnOff (getCoords start end) lights
  | otherwise = toggle (getCoords start end) lights

runLightLine :: String -> Map (Int, Int) Int -> Map (Int, Int) Int
runLightLine line = lightCommand command start end
  where
    command = extractCommand line
    coords = extractCoords line
    start = fst coords
    end = snd coords

extractCommand :: String -> String
extractCommand line
  | line =~ [re|^toggle|] = "toggle"
  | line =~ [re|^turn on|] = "on"
  | line =~ [re|^turn off|] = "off"

extractCoords :: String -> ((Int, Int), (Int, Int))
extractCoords line = (start, end)
  where
    coords = Prelude.map read $ matches (line *=~ [re|\d+|]) :: [Int]
    start = listToTuple $ take 2 coords
    end = listToTuple $ tail $ tail coords

listToTuple :: [Int] -> (Int, Int)
listToTuple [x, y] = (x, y)

processLines :: [String] -> Map (Int, Int) Int -> Map (Int, Int) Int
processLines docs lights = foldl (flip runLightLine) lights docs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  let lights = empty
  print $ Map.foldl' (+) 0 $ processLines s lights
