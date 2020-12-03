rowsWithTrees :: Int -> [[Bool]] -> [Bool]
rowsWithTrees _ [] = []
rowsWithTrees i (x:xs) = (x !! pos):rowsWithTrees (i + 1) xs
  where pos = (i * 3) `mod` length x

main :: IO ()
main = do
  input <- map (map (=='#')) . lines <$> readFile "input.txt" :: IO [[Bool]]
  print $ length $ filter id $ rowsWithTrees 0 input
