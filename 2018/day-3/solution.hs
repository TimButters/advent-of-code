import Data.Text (pack, strip, unpack)
import qualified Data.Set as Set

movement :: (Num a, Num b) => Char -> (a, b)
movement c
  | c == '>' = (1, 0)
  | c == '<' = (-1, 0)
  | c == '^' = (0, 1)
  | c == 'v' = (0, -1)

move :: (Num b, Num a) => (a, b) -> (a, b) -> (a, b)
move coord1 coord2 = (x, y)
  where
    x = fst coord1 + fst coord2
    y = snd coord1 + snd coord2

journey :: [Char] -> Set.Set (Int, Int)
journey directions = Set.fromList $ scanl move (0,0) movements
  where
    movements = map movement directions

takeOdds :: [a] -> [a]
takeOdds [] = []
takeOdds (x:xs) = x:takeEvens xs

takeEvens :: [a] -> [a]
takeEvens [] = []
takeEvens (x:xs) = takeOdds xs

splitJourney :: [Char] -> Set.Set (Int, Int)
splitJourney directions = Set.union santa roboSanta
  where
    santa = journey $ takeOdds directions
    roboSanta = journey $ takeEvens directions

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = unpack $ strip $ pack input
  print $ length $ journey s
  print $ length $ splitJourney s
