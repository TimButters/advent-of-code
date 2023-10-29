
import Data.Text (pack, strip, unpack)

charCount :: String -> Int
charCount s = count
    where
        base = length s
        backSlash =

main :: IO ()
main = do
  input <- readFile "test_input.txt"
  let s = unpack $ strip $ pack input
  let strings = lines s
  print $ sum . map length $ strings
