import Data.List (group)

sayAndSpeak :: String -> String
sayAndSpeak input = concatMap sayAndSpeakGroup parts
  where
    parts = group input

sayAndSpeakGroup :: String -> String
sayAndSpeakGroup group = show (length group) ++ [head group]

main :: IO ()
main = do
  let input = "1113222113"
  print $ length $ iterate sayAndSpeak input !! 40
  print $ length $ iterate sayAndSpeak input !! 50
