{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (Value (Array, Number, Object, String), decode)
import Data.Aeson.KeyMap (elems)
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (pack, splitOn, strip, unpack)
import Data.Vector qualified as V
import Text.RE.PCRE (ed, matches, re, (*=~), (*=~/), (=~))

sumAllNumbers :: String -> Int
sumAllNumbers s = sum $ map read $ matches (s *=~ [re|-?\d+|])

sumWithoutRed :: String -> Maybe Int
sumWithoutRed s = do
  toBoundedInteger $ addJson $ fromJust $ decode $ B.pack s
  where
    addJson (Number n) = n
    addJson (Array a) = sum $ map addJson $ V.toList a
    addJson (Object m) | String "red" `elem` elems m = 0
    addJson (Object m) = sum $ map addJson $ elems m
    addJson _ = 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let s = lines $ unpack $ strip $ pack input
  print $ sumAllNumbers input
  print $ sumWithoutRed input
