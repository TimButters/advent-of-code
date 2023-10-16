{-# LANGUAGE QuasiQuotes #-}

import Data.Text (pack, strip, unpack)
import Text.RE.PCRE (countMatches, re, (*=~), (=~))

isNice :: String -> Int
isNice s
  | s =~ [re|(ab|cd|pq|xy)|] = 0
  | (s =~ [re|(.)\1|]) && (countMatches (s *=~ [re|(a|e|i|o|u)|]) > 2) = 1
  | otherwise = 0

isNice2 :: String -> Int
isNice2 s
  | (s =~ [re|(..).*\1|]) && (s =~ [re|(.).\1|]) = 1
  | otherwise = 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  print $ sum $ map isNice s
  print $ sum $ map isNice2 s
