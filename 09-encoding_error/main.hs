findInvalid :: [Int] -> [Int] -> Int
findInvalid prev@(_:prevTail) (current:next)
  | isValid = findInvalid (prevTail ++ [current]) next
  | otherwise = current
    where isValid = any (\(a, b) -> a /= b && a + b == current) $ (,) <$> prev <*> prev

subsequence :: Int -> [Int] -> [Int]
subsequence n xs
  | length sums > 1 && last sums == n = take (length sums) xs
  | otherwise = subsequence n (tail xs)
    where sums = takeWhile (<= n) $ scanl1 (+) xs

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt" :: IO [Int]
  let invalid = findInvalid (take 25 input) (drop 25 input)
  print invalid
  let addends = subsequence invalid input
  print $ minimum addends + maximum addends
