import           Data.List (sort)

joltDeltas :: [Int] -> (Int, Int) -> (Int, Int)
joltDeltas [_] (deltas1, deltas3) = (deltas1, succ deltas3)
joltDeltas (x1:x2:xs) (deltas1, deltas3)
  | x2 - x1 == 1 = joltDeltas (x2:xs) (succ deltas1, deltas3)
  | x2 - x1 == 3 = joltDeltas (x2:xs) (deltas1, succ deltas3)

adjacency :: [Int] -> [[Int]]
adjacency xs = [[if reachable i j then 1 else 0 | j <- [0..size]] | i <- [0..size]]
  where
    reachable i j = elem j xs && (elem i xs || i == 0) && j > i && j - i <= 3
    size = maximum xs

matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult xs ys = [[sum $ zipWith (*) (xs !! i) (map (!! j) ys) | j <- [0..size]] | i <- [0..size]]
  where size = pred $ length xs

matrixAdd :: [[Int]] -> [[Int]] -> [[Int]]
matrixAdd xs ys = [zipWith (+) (xs !! i) (ys !! i) | i <- [0..pred $ length xs]]

main :: IO ()
main = do
  input <- (0:) . sort . map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ uncurry (*) $ joltDeltas input (0, 0)
  print $ last . head $ foldl1 matrixAdd $ scanl1 matrixMult (replicate (length input) $ adjacency input)
