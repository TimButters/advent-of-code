{-# LANGUAGE QuasiQuotes #-}

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Text (pack, splitOn, strip, unpack)
import Text.RE.PCRE (countMatches, re, (*=~), (=~))

parseInstructionLine :: String -> (String, (Maybe String, String, Maybe String))
parseInstructionLine line = (wire, (instruction, val1, val2))
  where
    wire = head . tail . head $ (line =~ [re|-> (.+)|] :: [[String]])
    instructionMatch = line =~ [re|[A-Z]+|] :: [[String]]
    instruction =
      if length instructionMatch == 1
        then Just (head . head $ instructionMatch)
        else Nothing
    vals = line =~ [re|(?<!-> )[a-z0-9]+(?!$)|] :: [[String]]
    val1 = head . head $ vals
    val2 =
      if length vals == 2
        then Just (head . head . tail $ vals)
        else Nothing

parseValueLine :: String -> (String, Int)
parseValueLine line = (wire, value)
  where
    vals = splitOn (pack "->") $ pack line
    value = read $ head $ map unpack vals :: Int
    wire = last $ map (unpack . strip) vals

parseLines :: [String] -> (Map String (Maybe String, String, Maybe String), Map String Int)
parseLines lines = (Map.fromList instructions, Map.fromList $ values ++ [(show x, x) | x <- [1 .. 10]])
  where
    instructions = map parseInstructionLine (filter (=~ [re|(^[a-z0-9]*[^A-Z]+$|^.*[A-Z]+.*$)|]) lines)
    values = map parseValueLine (filter (=~ [re|^\d+[^A-Z]+$|]) lines)

simulateCircuit :: String -> Map String Int -> Map String (Maybe String, String, Maybe String) -> Map String Int
simulateCircuit wire values instructions = newValues
  where
    newValues =
      if Map.member wire values
        then values
        else runGate wire values instructions

runGate :: String -> Map String Int -> Map String (Maybe String, String, Maybe String) -> Map String Int
runGate wire values instructions = Map.insert wire val newValues
  where
    (gate, input1, input2) = fromJust $ Map.lookup wire instructions
    newValues1 = simulateCircuit input1 values instructions
    newValues2
      | isNothing input2 || gate == Just "LSHIFT" || gate == Just "RSHIFT" = Map.empty
      | otherwise = simulateCircuit (fromJust input2) newValues1 instructions

    newValues = newValues1 `Map.union` newValues2

    val
      | isNothing gate = fromJust $ Map.lookup input1 newValues1
      | gate == Just "NOT" = complement . fromJust $ Map.lookup input1 newValues
      | gate == Just "AND" = (.&.) (fromJust (Map.lookup input1 newValues)) (fromJust (Map.lookup (fromJust input2) newValues))
      | gate == Just "OR" = (.|.) (fromJust (Map.lookup input1 newValues)) (fromJust (Map.lookup (fromJust input2) newValues))
      | gate == Just "LSHIFT" = shiftL (fromJust (Map.lookup input1 newValues)) (read (fromJust input2) :: Int)
      | gate == Just "RSHIFT" = shiftR (fromJust (Map.lookup input1 newValues)) (read (fromJust input2) :: Int)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  let parsed = parseLines s
  let instructions = fst parsed
  let values = snd parsed
  let simulation = simulateCircuit "a" values instructions
  let a = fromJust $ Map.lookup "a" simulation
  print $ "Part 1: " ++ show a
  let valuesP2 = Map.insert "b" a values
  let simulationP2 = simulateCircuit "a" valuesP2 instructions
  let aNew = fromJust $ Map.lookup "a" simulationP2
  print $ "Part 2: " ++ show aNew
