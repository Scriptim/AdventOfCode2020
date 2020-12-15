import           Data.List       (elemIndex)
import           Data.List.Split (splitOn)

nextNum :: [Int] -> [Int]
nextNum history@(x:xs) = case elemIndex x xs of
  Nothing -> 0:history
  Just n  -> succ n:history

main :: IO ()
main = do
  input <- reverse . map read . splitOn "," <$> readFile "input.txt" :: IO [Int]
  print $ head . foldl1 (.) (replicate (2020 - length input) nextNum) $ input
