import Data.Map (Map)
import qualified Data.Map as Map

data Ingredient = Ingredient
  { name :: String,
    capacity :: Int,
    durability :: Int,
    flavour :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

processLine :: String -> Ingredient
processLine s = Ingredient {name = name, capacity = capacity, durability = durability, flavour = flavour, texture = texture, calories = calories}
  where
    w = words s
    name = init . head $ w
    capacity = read . init $ w !! 2 :: Int
    durability = read . init $ w !! 4 :: Int
    flavour = read . init $ w !! 6 :: Int
    texture = read . init $ w !! 8 :: Int
    calories = read $ w !! 10 :: Int

scoreCookie :: [Ingredient] -> [Int] -> Int
scoreCookie ingredients portions = capacityScore * durabilityScore * flavourScore * textureScore
  where
    capacityScore = max 0 $ sum $ zipWith (*) (map capacity ingredients) portions
    durabilityScore = max 0 $ sum $ zipWith (*) (map durability ingredients) portions
    flavourScore = max 0 $ sum $ zipWith (*) (map flavour ingredients) portions
    textureScore = max 0 $ sum $ zipWith (*) (map texture ingredients) portions

main :: IO ()
main = do
  input <- readFile "test_input.txt"
  let s = lines input
  let ingredients = map processLine s
  print $ map (scoreCookie ingredients) [[42, 58], [43, 57], [44, 56], [45, 55], [46, 54]]
