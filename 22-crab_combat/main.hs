import           Data.List.Split (splitOn)

turn :: [Int] -> [Int] -> [Int]
turn xs [] = xs
turn [] ys = ys
turn (x:xs) (y:ys)
  | x > y = turn (xs ++ [x, y]) ys
  | x < y = turn xs (ys ++ [y, x])

main :: IO ()
main = do
  [player1, player2] <- map (map read . tail . lines) . splitOn "\n\n" <$> readFile "input.txt" :: IO [[Int]]
  print $ sum . zipWith (*) [1..] . reverse $ turn player1 player2
