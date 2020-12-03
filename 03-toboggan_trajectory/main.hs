rowsWithTrees :: Int -> Int -> Int -> [[Bool]] -> [Bool]
rowsWithTrees _ _ _ [] = []
rowsWithTrees right down i (x:xs) = (x !! pos):rowsWithTrees right down (i + 1) (drop (down - 1) xs)
  where pos = (i * right) `mod` length x

countTrees :: [Bool] -> Int
countTrees = length . filter id

main :: IO ()
main = do
  input <- map (map (=='#')) . lines <$> readFile "input.txt" :: IO [[Bool]]
  print $ countTrees $ rowsWithTrees 3 1 0 input
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print $ product $ map (\(r, d) -> countTrees $ rowsWithTrees r d 0 input) slopes
