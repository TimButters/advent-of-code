import Data.Digest.Pure.MD5 (md5)
import Data.Text.Lazy qualified as TL (pack)
import Data.Text.Lazy.Encoding as TE (encodeUtf8)

suffixHash :: [Char] -> Int -> (String, Int)
suffixHash s n = (show $ md5 $ TE.encodeUtf8 $ TL.pack (s ++ show n), n)

leadingZeros :: Int -> [Char] -> Bool
leadingZeros n s = take n s == replicate n '0'

findSuffix :: Int -> [Char] -> Int
findSuffix n s = suffix
    where
        ans = head $ filter (leadingZeros n . fst) $ map (suffixHash s) [1..]
        suffix = snd ans

main :: IO ()
main = do
  print $ findSuffix 5 "yzbqklnj"
  print $ findSuffix 6 "yzbqklnj"
