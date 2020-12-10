import           Data.List (sort)

joltDeltas :: [Int] -> (Int, Int) -> (Int, Int)
joltDeltas [_] (deltas1, deltas3) = (deltas1, succ deltas3)
joltDeltas (x1:x2:xs) (deltas1, deltas3)
  | x2 - x1 == 1 = joltDeltas (x2:xs) (succ deltas1, deltas3)
  | x2 - x1 == 3 = joltDeltas (x2:xs) (deltas1, succ deltas3)

main :: IO ()
main = do
  input <- sort . map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ uncurry (*) $ joltDeltas (0:input) (0, 0)
