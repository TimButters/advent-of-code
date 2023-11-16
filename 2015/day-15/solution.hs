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

scoreCookie :: [Ingredient] -> Bool -> [Int] -> Int
scoreCookie ingredients calorieLimit portions =
  if not calorieLimit || (totalCalories == 500)
    then capacityScore * durabilityScore * flavourScore * textureScore
    else 0
  where
    capacityScore = max 0 $ sum $ zipWith (*) (map capacity ingredients) portions
    durabilityScore = max 0 $ sum $ zipWith (*) (map durability ingredients) portions
    flavourScore = max 0 $ sum $ zipWith (*) (map flavour ingredients) portions
    textureScore = max 0 $ sum $ zipWith (*) (map texture ingredients) portions
    totalCalories = sum $ zipWith (*) (map calories ingredients) portions

trialWeights :: [[Int]]
trialWeights = [[w1, w2, w3, w4] | w1 <- [0 .. 50], w2 <- [0 .. 50], w3 <- [0 .. 50], w4 <- [0 .. 50], w1 + w2 + w3 + w4 == 100]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines input
  let ingredients = map processLine s
  print $ (maximum . map (scoreCookie ingredients False)) trialWeights
  print $ (maximum . map (scoreCookie ingredients True)) trialWeights
