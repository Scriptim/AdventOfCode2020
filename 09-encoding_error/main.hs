findInvalid :: [Int] -> [Int] -> Int
findInvalid prev@(_:prevTail) (current:next)
  | isValid = findInvalid (prevTail ++ [current]) next
  | otherwise = current
    where isValid = any (\(a, b) -> a /= b && a + b == current) $ (,) <$> prev <*> prev

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ findInvalid (take 25 input) (drop 25 input)
