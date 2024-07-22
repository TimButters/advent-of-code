import Data.List (foldl', sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack, replace, unpack, Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

parseReplacements :: [String] -> Map String [String]
parseReplacements = foldl' (\r s -> Map.insertWith (++) (head $ words s) [last $ words s] r) Map.empty

replaceMolecule :: Text -> Text -> Text -> [Text]
replaceMolecule needle molecule replacement = newMolecules
  where
    splits = T.breakOnAll needle molecule
    newMolecules = map (\(st, en) -> st <> replacement <> T.drop (T.length needle) en) splits

replaceMolecules :: Map String [String] -> String -> Set Text
replaceMolecules replacements molecule = Set.fromList $ concat . concat $ distinctMolecules
  where
    distinctMolecules = Map.elems $ Map.mapWithKey (\k v -> map (replaceMolecule (pack k) (pack molecule) . pack) v) replacements

sortKeys :: Map String [String] -> [String]
sortKeys m = sortBy cmp (Map.keys m)
  where
    cmp a b
      | length a < length b = GT
      | length a == length b = EQ
      | otherwise = LT

invertMap :: Map String [String] -> Map String [String]
invertMap m = Map.fromListWith (++) pairs
    where pairs = [(v, [k]) | (k, vs) <- Map.toList m, v <- vs]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines input
  let start = last s
  let replacements = parseReplacements $ init . init $ s
  print $ length $ replaceMolecules replacements start
  print $ sortKeys replacements
  print replacements
