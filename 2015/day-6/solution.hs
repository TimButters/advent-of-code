{-# LANGUAGE QuasiQuotes #-}

import Data.Set (Set, difference, fromList, member, notMember, union, empty, size)
import Data.Text (pack, strip, unpack)
import Text.RE.PCRE (countMatches, re, (*=~), (=~), matches)
import qualified Control.Applicative as Set

getCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getCoords start end = [(x, y) | x <- [xStart .. xEnd], y <- [yStart .. yEnd]]
  where
    xStart = fst start
    yStart = snd start
    xEnd = fst end
    yEnd = snd end

turnOn :: Ord a => [a] -> Set a -> Set a
turnOn coords lights = lights `union` fromList coords

turnOff :: Ord a => [a] -> Set a -> Set a
turnOff coords lights = lights `difference` fromList coords

toggle :: Ord a => [a] -> Set a -> Set a
toggle coords lights = turnOn (filter (`notMember` lights) coords) (turnOff (filter (`member` lights) coords) lights)

lightCommand :: String -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
lightCommand command start end lights
  | command == "on" = turnOn (getCoords start end) lights
  | command == "off" = turnOff (getCoords start end) lights
  | otherwise = toggle (getCoords start end) lights

runLightLine :: String -> Set (Int, Int) -> Set (Int, Int)
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
    coords = map read $ matches (line *=~ [re|\d+|]) :: [Int]
    start = listToTuple $ take 2 coords
    end = listToTuple $ tail $ tail coords

listToTuple :: [Int] -> (Int, Int)
listToTuple [x,y] = (x,y)

processLines :: [String] -> Set (Int, Int) -> Set (Int, Int)
--processLines [] lights = lights
--processLines (x:xs) lights = processLines xs (runLightLine x lights)
processLines docs lights = foldl (flip runLightLine) lights docs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  print $ size $ processLines s empty
