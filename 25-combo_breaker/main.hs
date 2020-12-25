import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

loopSize :: Int -> Int
loopSize subject = fromJust . elemIndex subject $ scanl (\accu _ -> accu * 7 `mod` 20201227) 1 [1..]

transform :: Int -> Int -> Int
transform loopSize num = foldl1 (.) (replicate loopSize (\accu -> accu * num `mod` 20201227)) 1

main :: IO ()
main = do
  publicKeys <- map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ transform (loopSize $ head publicKeys) (last publicKeys)
