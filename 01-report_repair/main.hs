get2020addends :: [Int] -> (Int, Int)
get2020addends xs = head $ filter (\(x, y) -> x + y == 2020) $ (,) <$> xs <*> xs

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt" :: IO [Int]
  print $ uncurry (*) $ get2020addends input
