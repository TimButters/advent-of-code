{-# LANGUAGE MultiWayIf #-}

import Data.Maybe (fromJust, isNothing)
import Data.Text (Text, pack, splitOn, unpack)

data Aunt = Aunt
  { index :: Int,
    children :: Maybe Int,
    cats :: Maybe Int,
    samoyeds :: Maybe Int,
    pomeranians :: Maybe Int,
    akitas :: Maybe Int,
    vizslas :: Maybe Int,
    goldfish :: Maybe Int,
    trees :: Maybe Int,
    cars :: Maybe Int,
    perfumes :: Maybe Int
  }
  deriving (Show)

toTuple :: [Text] -> (String, Int)
toTuple [a, b] = (unpack a, read (unpack b) :: Int)

buildAunt :: String -> Aunt
buildAunt s =
  Aunt
    { index = idx,
      children = children,
      cats = cats,
      samoyeds = samoyeds,
      pomeranians = pomeranians,
      akitas = akitas,
      vizslas = vizslas,
      goldfish = goldfish,
      trees = trees,
      cars = cars,
      perfumes = perfumes
    }
  where
    idx = read $ init $ words s !! 1 :: Int
    body = concat $ drop 2 (words s)
    items = map (toTuple . splitOn (pack ":")) $ splitOn (pack ",") (pack body)
    children = lookup "children" items
    cats = lookup "cats" items
    samoyeds = lookup "samoyeds" items
    pomeranians = lookup "pomeranians" items
    akitas = lookup "akitas" items
    vizslas = lookup "vizslas" items
    goldfish = lookup "goldfish" items
    trees = lookup "trees" items
    cars = lookup "cars" items
    perfumes = lookup "perfumes" items

filterAunts :: (Aunt -> Maybe Int) -> Int -> [Aunt] -> [Aunt]
filterAunts field quantity =
  concatMap
    ( \aunt ->
        if
            | (isNothing (field aunt)) -> [aunt]
            | field aunt == Just quantity -> [aunt]
            | otherwise -> []
    )

filterAunts2 :: (Aunt -> Maybe Int) -> String -> Int -> [Aunt] -> [Aunt]
filterAunts2 field comp quantity =
  concatMap
    ( \aunt ->
        if
            | (isNothing (field aunt)) -> [aunt]
            | comp == "<" && field aunt < Just quantity -> [aunt]
            | comp == ">" && field aunt > Just quantity -> [aunt]
            | comp == "==" && field aunt == Just quantity -> [aunt]
            | otherwise -> []
    )

main :: IO ()
main =
  do
    input <- readFile "input.txt"
    let s = lines input
    let aunts = map buildAunt s
    print $ index . head
      $ filterAunts children 3
      $ filterAunts cats 7
      $ filterAunts samoyeds 2
      $ filterAunts pomeranians 3
      $ filterAunts akitas 0
      $ filterAunts vizslas 0
      $ filterAunts goldfish 5
      $ filterAunts trees 3
      $ filterAunts cars 2
      $ filterAunts perfumes 1 aunts

    print $ index . head
      $ filterAunts2 children "==" 3
      $ filterAunts2 cats ">" 7
      $ filterAunts2 samoyeds "==" 2
      $ filterAunts2 pomeranians "<" 3
      $ filterAunts2 akitas "==" 0
      $ filterAunts2 vizslas "==" 0
      $ filterAunts2 goldfish "<" 5
      $ filterAunts2 trees ">" 3
      $ filterAunts2 cars "==" 2
      $ filterAunts2 perfumes "==" 1 aunts
